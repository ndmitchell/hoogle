{-# LANGUAGE RecordWildCards #-}

module General.Timing(Timing, withTiming, timed) where

import Data.List.Extra
import System.Time.Extra
import Data.IORef
import Data.Maybe
import Control.Monad.Extra
import System.IO
import General.Util
import GHC.Stats
import Control.Monad.IO.Class


newtype Timing = Timing (Maybe (IORef [(String, Double)]))

withTiming :: Maybe FilePath -> (Timing -> IO a) -> IO a
withTiming file f = do
    offset <- offsetTime
    ref <- newIORef []
    res <- f $ Timing $ if isJust file then Just ref else Nothing
    end <- offset
    whenJust file $ \file -> do
        ref <- readIORef ref
        -- Expecting unrecorded of ~2s
        -- Most of that comes from the pipeline - we get occasional 0.01 between items as one flushes
        -- Then at the end there is ~0.5 while the final item flushes
        ref <- return $ reverse $ sortOn snd $ ("Unrecorded",end - sum (map snd ref)) : ref
        writeFile file $ unlines $ prettyTable 2 "Secs" ref
    putStrLn $ "Took " ++ showDuration end
    return res


timed :: MonadIO m => Timing -> String -> m a -> m a
timed (Timing ref) msg act = do
    liftIO $ putStr (msg ++ "... ") >> hFlush stdout
    time <- liftIO offsetTime
    res <- act
    time <- liftIO time
    stats <- liftIO getGCStatsEnabled
    s <- if not stats then return "" else do GCStats{..} <- liftIO getGCStats; return $ " (" ++ show peakMegabytesAllocated ++ "Mb)"
    liftIO $ putStrLn $ showDuration time ++ s
    case ref of -- don't use whenJust, induces Appliative pre-AMP
        Nothing -> return ()
        Just ref -> liftIO $ modifyIORef ref ((msg,time):)
    return res
