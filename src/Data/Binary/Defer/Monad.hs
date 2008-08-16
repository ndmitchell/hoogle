
module Data.Binary.Defer.Monad where

import System.IO
import System.IO.Unsafe
import Data.Binary.Raw
import Control.Monad.Reader
import Data.IORef
import qualified Data.ByteString as BS

import Data.Typeable
import qualified Data.TypeMap as TypeMap


---------------------------------------------------------------------
-- Defer Put

-- Storing the position explicitly gives a ~5% speed up
-- and removes hGetPos as being a bottleneck
-- possibly still not worth it though

type DeferPut a = ReaderT (Handle, IORef Int, IORef [DeferPending], IORef [DeferPatchup]) IO a
data DeferPending = DeferPending !Int (DeferPut ())
data DeferPatchup = DeferPatchup !Int !Int -- a b = at position a, write out b

putValue :: (Handle -> a -> IO ()) -> Int -> a -> DeferPut ()
putValue f size x = do
    (h,p,_,_) <- ask
    lift $ do
        modifyIORef p (+size)
        f h x

putInt, putByte :: Int -> DeferPut ()
putInt  = putValue hPutInt  4
putByte = putValue hPutByte 1

putChr :: Char -> DeferPut ()
putChr  = putValue hPutChar 1

putByteString :: BS.ByteString -> DeferPut ()
putByteString x = do
    let len = BS.length x
    putInt len
    putValue BS.hPut len x

putDefer :: DeferPut () -> DeferPut ()
putDefer x = do
    (h,p,ref,_) <- ask
    lift $ do
        p2 <- readIORef p
        hPutInt h 0 -- to backpatch
        modifyIORef p (+4)
        modifyIORef ref (DeferPending p2 x :)

runDeferPut :: Handle -> DeferPut () -> IO ()
runDeferPut h m = do
    ref <- newIORef []
    back <- newIORef []
    i <- hGetPos h
    p <- newIORef $ fromInteger i
    runReaderT (m >> runDeferPendings) (h,p,ref,back)
    patch <- readIORef back
    mapM_ (\(DeferPatchup a b) -> do hSetPos h (toInteger a); hPutInt h b) patch


runDeferPendings :: DeferPut ()
runDeferPendings = do
    (h,_,ref,back) <- ask
    todo <- lift $ readIORef ref
    lift $ writeIORef ref []
    mapM_ runDeferPending todo


-- TODO: Write into a Ptr buffer, then use hPutBuf to do the actual writing out
--       Should save lots of openning the file etc
runDeferPending :: DeferPending -> DeferPut ()
runDeferPending (DeferPending pos act) = do
    (h,p,_,back) <- ask
    lift $ do
        p2 <- readIORef p
        modifyIORef back (DeferPatchup pos p2 :)
    act
    runDeferPendings


---------------------------------------------------------------------
-- Defer Get

type DeferGet a = ReaderT (Handle, IORef TypeMap.TypeMap) IO a

getInt, getByte :: DeferGet Int
getInt  = do h <- asks fst; lift $ hGetInt  h
getByte = do h <- asks fst; lift $ hGetByte h

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
