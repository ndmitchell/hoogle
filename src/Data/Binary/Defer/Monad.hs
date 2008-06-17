
module Data.Binary.Defer.Monad where

import System.IO
import System.IO.Unsafe
import Data.Binary.Raw
import Control.Monad.Reader
import Data.IORef


---------------------------------------------------------------------
-- Defer Put

type DeferPut a = ReaderT (Handle, IORef [DeferPending]) IO a
data DeferPending = DeferPending Int (DeferPut ())

putInt, putByte :: Int -> DeferPut ()
putInt  x = do h <- asks fst; lift $ hPutInt  h x
putByte x = do h <- asks fst; lift $ hPutByte h x

putChr :: Char -> DeferPut ()
putChr  x = do h <- asks fst; lift $ hPutChar h x

putDefer :: DeferPut () -> DeferPut ()
putDefer x = do
    (h,ref) <- ask
    p <- lift $ hGetPos h
    lift $ hPutInt h 0 -- to backpatch
    lift $ modifyIORef ref (DeferPending p x :)

runDeferPut :: Handle -> DeferPut () -> IO ()
runDeferPut h m = do
    ref <- newIORef []
    runReaderT m (h,ref)
    todo <- readIORef ref
    mapM_ (runDeferPending h) (reverse todo)

runDeferPending :: Handle -> DeferPending -> IO ()
runDeferPending h (DeferPending pos act) = do
    i <- hGetPos h
    hSetPos h pos
    hPutInt h i
    hSetPos h i
    runDeferPut h act


---------------------------------------------------------------------
-- Defer Get

type DeferGet a = ReaderT Handle IO a

getInt, getByte :: DeferGet Int
getInt  = do h <- ask; lift $ hGetInt  h
getByte = do h <- ask; lift $ hGetByte h

getChr :: DeferGet Char
getChr  = do h <- ask; lift $ hGetChar h

getDefer :: DeferGet a -> DeferGet a
getDefer x = do
    h <- ask
    i <- lift $ hGetInt h
    lift $ unsafeInterleaveIO $ do
        hSetPos h i
        runDeferGet h x

runDeferGet :: Handle -> DeferGet a -> IO a
runDeferGet = flip runReaderT
