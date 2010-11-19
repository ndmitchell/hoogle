
module Data.Binary.Defer.Monad(
    DeferPut, putDefer, runDeferPut,
    putInt, putByte, putChr, putByteString, putLazyByteString,
    DeferGet, getDefer, runDeferGet,
    getInt, getByte, getChr, getByteString, getLazyByteString,
    getDeferGet, getDeferPut
    ) where

import System.IO
import System.IO.Unsafe
import Data.Binary.Raw
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Typeable
import qualified Data.TypeMap as TypeMap
import Foreign


---------------------------------------------------------------------
-- Defer Put

-- Storing the position explicitly gives a ~5% speed up
-- and removes hGetPos as being a bottleneck
-- possibly still not worth it though

-- FIXME: Should be a newtype
type DeferPut a = ReaderT (Buffer, IORef [DeferPending], IORef [DeferPatchup]) IO a
data DeferPending = DeferPending !Int (DeferPut ())
data DeferPatchup = DeferPatchup !Int !Int -- a b = at position a, write out b

putValue :: Storable a => a -> DeferPut ()
putValue x = do
    (buf,_,_) <- ask
    liftIO $ bufferAdd buf x

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
    liftIO $ bufferAddByteString buf x

putLazyByteString :: LBS.ByteString -> DeferPut ()
putLazyByteString x = do
    (buf,_,_) <- ask
    putInt $ fromIntegral $ LBS.length x
    liftIO $ bufferAddLazyByteString buf x

putDefer :: DeferPut () -> DeferPut ()
putDefer x = do
    (buf,ref,_) <- ask
    liftIO $ do
        p <- bufferPos buf
        bufferAdd buf (0 :: Int32) -- to backpatch
        modifyIORef ref (DeferPending p x :)

runDeferPut :: Handle -> DeferPut () -> IO ()
runDeferPut h m = do
    buf <- bufferNew h
    ref <- newIORef []
    back <- newIORef []
    runReaderT (m >> runDeferPendings) (buf,ref,back)
    bufferFlush buf
    patch <- readIORef back
    mapM_ (\(DeferPatchup a b) -> do hSetPos h (toInteger a); hPutInt h b) patch


runDeferPendings :: DeferPut ()
runDeferPendings = do
    (_,ref,back) <- ask
    todo <- liftIO $ readIORef ref
    liftIO $ writeIORef ref []
    mapM_ runDeferPending todo


runDeferPending :: DeferPending -> DeferPut ()
runDeferPending (DeferPending pos act) = do
    (buf,_,back) <- ask
    liftIO $ do
        p <- bufferPos buf
        b <- bufferPatch buf pos (fromIntegral p :: Int32)
        unless b $ modifyIORef back (DeferPatchup pos p :)
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


bufferAddLazyByteString :: Buffer -> LBS.ByteString -> IO ()
bufferAddLazyByteString (Buffer h file ptr buf) x = do
    let sz = fromIntegral $ LBS.length x
    buf2 <- readIORef buf
    when (buf2 /= 0) $ do
        hPutBuf h ptr buf2
        writeIORef buf 0
    modifyIORef file (+ (buf2+sz)) 
    LBS.hPut h x


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


-- Patch at position p, with value v
-- Return True if you succeeded
bufferPatch :: Buffer -> Int -> Int32 -> IO Bool
bufferPatch (Buffer h file ptr buf) p v = do
    i <- readIORef file
    if p < i then return False else do
        pokeByteOff ptr (p-i) v
        return True

---------------------------------------------------------------------
-- Defer Get

type DeferGet a = ReaderT (Handle, IORef TypeMap.TypeMap) IO a

getInt :: DeferGet Int
getInt  = do h <- asks fst; liftIO $ hGetInt  h

getByte :: DeferGet Word8
getByte = do h <- asks fst; liftIO $ liftM fromIntegral $ hGetByte h

getChr :: DeferGet Char
getChr  = do h <- asks fst; liftIO $ hGetChar h

getByteString :: DeferGet BS.ByteString
getByteString = do
    h <- asks fst
    len <- liftIO $ hGetInt h
    liftIO $ BS.hGet h len

getLazyByteString :: DeferGet LBS.ByteString
getLazyByteString = do
    h <- asks fst
    len <- liftIO $ hGetInt h
    liftIO $ LBS.hGet h $ fromIntegral len

getDefer :: DeferGet a -> DeferGet a
getDefer x = do
    h <- asks fst
    i <- liftIO $ hGetInt h
    s <- ask
    liftIO $ unsafeInterleaveIO $ do
        hSetPos h (toInteger i)
        runReaderT x s

runDeferGet :: Handle -> DeferGet a -> IO a
runDeferGet h m = do
    ref <- newIORef TypeMap.empty
    runReaderT m (h,ref)


getDeferGet :: Typeable a => DeferGet a
getDeferGet = do
    ref <- asks snd
    mp <- liftIO $ readIORef ref
    return $ TypeMap.find mp

getDeferPut :: Typeable a => a -> DeferGet ()
getDeferPut x = do
    ref <- asks snd
    liftIO $ modifyIORef ref $ TypeMap.insert x
