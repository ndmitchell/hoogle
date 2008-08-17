
module Data.Binary.Defer.Monad(
    DeferPut, putDefer, runDeferPut,
    putInt, putByte, putChr, putByteString,
    DeferGet, getDefer, runDeferGet,
    getInt, getByte, getChr, getByteString,
    getDeferGet, getDeferPut
    ) where

import System.IO
import System.IO.Unsafe
import Data.Binary.Raw
import Control.Monad.Reader
import Data.IORef
import qualified Data.ByteString as BS

import Data.Typeable
import qualified Data.TypeMap as TypeMap
import Foreign


---------------------------------------------------------------------
-- Defer Put

-- Storing the position explicitly gives a ~5% speed up
-- and removes hGetPos as being a bottleneck
-- possibly still not worth it though

type DeferPut a = ReaderT (Buffer, IORef [DeferPending], IORef [DeferPatchup]) IO a
data DeferPending = DeferPending !Int (DeferPut ())
data DeferPatchup = DeferPatchup !Int !Int -- a b = at position a, write out b

putValue :: Storable a => a -> DeferPut ()
putValue x = do
    (buf,_,_) <- ask
    lift $ bufferAdd buf x

putInt :: Int -> DeferPut ()
putInt x = putValue (fromIntegral x :: Int32)

putByte :: Word8 -> DeferPut ()
putByte x = putValue x

putChr :: Char -> DeferPut ()
putChr  x = putByte $ fromIntegral $ fromEnum x

putByteString :: BS.ByteString -> DeferPut ()
putByteString x = do
    (buf,_,_) <- ask
    putInt $ BS.length x
    lift $ bufferAddByteString buf x

putDefer :: DeferPut () -> DeferPut ()
putDefer x = do
    (buf,ref,_) <- ask
    lift $ do
        p <- bufferPos buf
        bufferAdd buf (0 :: Int32) -- to backpatch
        modifyIORef ref (DeferPending p x :)

runDeferPut :: Handle -> DeferPut () -> IO ()
runDeferPut h m = do
    buf <- bufferNew h
    ref <- newIORef []
    back <- newIORef []
    runReaderT (m >> runDeferPendings) (buf,ref,back)
    patch <- readIORef back
    mapM_ (\(DeferPatchup a b) -> do hSetPos h (toInteger a); hPutInt h b) patch


runDeferPendings :: DeferPut ()
runDeferPendings = do
    (_,ref,back) <- ask
    todo <- lift $ readIORef ref
    lift $ writeIORef ref []
    mapM_ runDeferPending todo


-- TODO: Write into a Ptr buffer, then use hPutBuf to do the actual writing out
--       Should save lots of openning the file etc
runDeferPending :: DeferPending -> DeferPut ()
runDeferPending (DeferPending pos act) = do
    (buf,_,back) <- ask
    lift $ do
        p <- bufferPos buf
        modifyIORef back (DeferPatchup pos p :)
    act
    runDeferPendings


---------------------------------------------------------------------
-- Buffer for writing

bufferSize = 10000 :: Int

-- (number in file, number in buffer)
data Buffer = Buffer !Handle !(IORef Int) !(Ptr ()) !(IORef Int)


bufferNew :: Handle -> IO Buffer
bufferNew h = do
    i <- hGetPos h
    file <- newIORef $ fromInteger i
    buf <- newIORef 0
    ptr <- mallocBytes bufferSize
    return $ Buffer h file ptr buf


bufferAdd :: Storable a => Buffer -> a -> IO ()
bufferAdd (Buffer h file ptr buf) x = do
    let sz = sizeOf x
    buf2 <- readIORef buf
    if sz + buf2 >= bufferSize then do
        hPutBuf h ptr buf2
        pokeByteOff ptr 0 x
        modifyIORef file (+buf2)
        writeIORef buf sz
     else do
        pokeByteOff ptr buf2 x
        writeIORef buf (buf2+sz)


bufferAddByteString :: Buffer -> BS.ByteString -> IO ()
bufferAddByteString (Buffer h file ptr buf) x = do
    let sz = BS.length x
    buf2 <- readIORef buf
    when (buf2 /= 0) $ do
        hPutBuf h ptr buf2
        writeIORef buf 0
    modifyIORef file (+ (buf2+sz)) 
    BS.hPut h x


bufferFlush :: Buffer -> IO ()
bufferFlush (Buffer h file ptr buf) = do
    buf2 <- readIORef buf
    hPutBuf h ptr buf2
    modifyIORef file (+buf2)
    writeIORef buf 0


bufferPos :: Buffer -> IO Int
bufferPos (Buffer h file ptr buf) = do
    i <- readIORef file
    j <- readIORef buf
    return $ i + j


---------------------------------------------------------------------
-- Defer Get

type DeferGet a = ReaderT (Handle, IORef TypeMap.TypeMap) IO a

getInt :: DeferGet Int
getInt  = do h <- asks fst; lift $ hGetInt  h

getByte :: DeferGet Word8
getByte = do h <- asks fst; lift $ liftM fromIntegral $ hGetByte h

getChr :: DeferGet Char
getChr  = do h <- asks fst; lift $ hGetChar h

getByteString :: DeferGet BS.ByteString
getByteString = do
    h <- asks fst
    len <- lift $ hGetInt h
    lift $ BS.hGet h len

getDefer :: DeferGet a -> DeferGet a
getDefer x = do
    h <- asks fst
    i <- lift $ hGetInt h
    s <- ask
    lift $ unsafeInterleaveIO $ do
        hSetPos h (toInteger i)
        runReaderT x s

runDeferGet :: Handle -> DeferGet a -> IO a
runDeferGet h m = do
    ref <- newIORef TypeMap.empty
    runReaderT m (h,ref)


getDeferGet :: Typeable a => DeferGet a
getDeferGet = do
    ref <- asks snd
    mp <- lift $ readIORef ref
    return $ TypeMap.find mp

getDeferPut :: Typeable a => a -> DeferGet ()
getDeferPut x = do
    ref <- asks snd
    lift $ modifyIORef ref $ TypeMap.insert x
