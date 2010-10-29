
module General.Parallel(parallelN_, parallel_) where

import Control.Concurrent
import Control.Exception
import Control.Monad


parallelN_ :: Int -> [IO ()] -> IO ()
parallelN_ i xs | i <= 1 = sequence_ xs
parallelN_ i xs = do
    todo <- newMVar xs
    parallel_ $ replicate i $ f todo
    where
        f todo = do
            x <- takeMVar todo
            case x of
                [] -> putMVar todo [] >> return ()
                x:xs -> putMVar todo xs >> x >> f todo


parallel_ :: [IO ()] -> IO ()
parallel_ xs = do
    done <- forM xs $ \x -> do
        m <- newEmptyMVar
        forkIO (x `finally` putMVar m ())
        return m
    mapM_ takeMVar done
