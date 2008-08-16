
module Data.Binary.Defer.Monad where

import System.IO
import System.IO.Unsafe
import Data.Binary.Raw
import Control.Monad.Reader
import Data.IORef
import Data.ByteString as BS

import Data.Typeable
import qualified Data.TypeMap as TypeMap


---------------------------------------------------------------------
-- Defer Put

-- Storing the position explicitly gives a ~5% speed up
-- and removes hGetPos as being a bottleneck
-- possibly still not worth it though

type DeferPut a = ReaderT (Handle, IORef Int, IORef [DeferPending]) IO a
data DeferPending = DeferPending Int (DeferPut ())

putValue :: (Handle -> a -> IO ()) -> Int -> a -> DeferPut ()
putValue f size x = do
    (h,p,_) <- ask
    lift $ do
        modifyIORef p (+size)
        f h x

putInt, putByte :: Int -> DeferPut ()
putInt  = putValue hPutInt  4
putByte = putValue hPutByte 1

putChr :: Char -> DeferPut ()
putChr  = putValue hPutChar 1

putByteString :: ByteString -> DeferPut ()
putByteString x = do
    let len = BS.length x
    putInt len
    putValue hPut len x

putDefer :: DeferPut () -> DeferPut ()
putDefer x = do
    (h,p,ref) <- ask
    lift $ do
        p2 <- readIORef p
        hPutInt h 0 -- to backpatch
        modifyIORef p (+4)
        modifyIORef ref (DeferPending p2 x :)

runDeferPut :: Handle -> DeferPut () -> IO ()
runDeferPut h m = do
    ref <- newIORef []
    i <- hGetPos h
    p <- newIORef $ fromInteger i
    runReaderT (m >> runDeferPendings) (h,p,ref)


runDeferPendings :: DeferPut ()
runDeferPendings = do
    (h,_,ref) <- ask
    todo <- lift $ readIORef ref
    lift $ writeIORef ref []
    mapM_ runDeferPending todo


-- TODO: Idea, have a set of pending objects
--       Which are written out serially, should be very cache/pos friendly
--       Then accumulate a list of backpatches, which are written out at the
--       very end. i.e. two defer sets, move from set 1 to set 2, then action
--       Will save one hSetPos per update, plus give much better cache performance (hopefully)
runDeferPending :: DeferPending -> DeferPut ()
runDeferPending (DeferPending pos act) = do
    (h,p,_) <- ask
    lift $ do
        p2 <- readIORef p
        hSetPos h (toInteger pos)
        hPutInt h p2
        hSetPos h (toInteger p2)
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

getByteString :: DeferGet ByteString
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
