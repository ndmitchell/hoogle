{-# LANGUAGE RecordWildCards #-}

module General.Timing(Timing, withTiming, timed, timedOverwrite) where

import Data.List.Extra
import System.Time.Extra
import Data.IORef
import Control.Monad.Extra
import System.IO
import General.Util
import Control.Monad.IO.Class

-- | A mutable object to keep timing information
data Timing = Timing
    { -- | Get time since the initialization of this 'Timing'.
      timingOffset :: IO Seconds
      -- | Record timings for writing to a file
    , timingStore :: IORef [(String, Seconds)]
      -- | If you are below T you may overwrite N characters
      -- at the end of the current terminal output.
      -- Only used iff @timingTerminal == True@.
    , timingOverwrite :: IORef (Maybe (Seconds, Int))
      -- | whether is this a terminal
    , timingTerminal :: Bool
    }

-- | Time an action, printing timing information to the terminal
withTiming ::
  -- | A file to optionally write all timings to, after the action is finished
  Maybe FilePath ->
  -- | An action that can write timings into 'Timing'
  (Timing -> IO a) ->
  IO a
withTiming writeTimingsTo act = do
    timingOffset <- offsetTime
    timingStore <- newIORef []
    timingOverwrite <- newIORef Nothing
    timingTerminal <- hIsTerminalDevice stdout

    res <- act Timing{..}
    total <- timingOffset
    whenJust writeTimingsTo $ \file -> do
        xs <- readIORef timingStore
        -- Expecting unrecorded of ~2s
        -- Most of that comes from the pipeline - we get occasional 0.01 between items as one flushes
        -- Then at the end there is ~0.5 while the final item flushes
        xs <- pure $ sortOn (negate . snd) $ ("Unrecorded", total - sum (map snd xs)) : xs
        writeFile file $ unlines $ prettyTable 2 "Secs" xs
    putStrLn $ "Took " ++ showDuration total
    pure res


-- skip it if have written out in the last 1s and takes < 0.1

-- | Time & write the given message to stdout
timed :: MonadIO m => Timing -> String -> m a -> m a
timed = timedEx False

-- | Time & write the given message to stdout
-- overwriting a previous message if it was marked as overwritable
timedOverwrite :: MonadIO m => Timing -> String -> m a -> m a
timedOverwrite = timedEx True

timedEx :: MonadIO m => Bool -> Timing -> String -> m a -> m a
timedEx overwrite Timing{..} msg act = do
    start <- liftIO timingOffset
    liftIO $ whenJustM (readIORef timingOverwrite) $ \(t,n) ->
        if overwrite && start < t then
            putStr $ replicate n '\b' ++ replicate n ' ' ++ replicate n '\b'
        else
            putStrLn ""

    let out msg = liftIO $ putStr msg >> pure (length msg)
    undo1 <- out $ msg ++ "... "
    liftIO $ hFlush stdout

    res <- act
    end <- liftIO timingOffset
    let time = end - start
    liftIO $ modifyIORef timingStore ((msg,time):)

    s <- maybe "" (\x -> " (" ++ x ++ ")") <$> liftIO getStatsPeakAllocBytes
    undo2 <- out $ showDuration time ++ s

    old <- liftIO $ readIORef timingOverwrite
    let next = maybe (start + 1.0) fst old
    liftIO $ if timingTerminal && overwrite && end < next then
        writeIORef timingOverwrite $ Just (next, undo1 + undo2)
     else do
        writeIORef timingOverwrite Nothing
        putStrLn ""
    pure res
