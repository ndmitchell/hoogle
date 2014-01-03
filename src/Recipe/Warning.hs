
module Recipe.Warning(
    withWarnings
    ) where

import General.System
import Control.Concurrent


withWarnings :: (([String] -> IO ()) -> IO ()) -> IO (Int, FilePath)
withWarnings act = do
    count <- newMVar 0
    (file, handle) <- openTempFile "" "hoogle_errors_.txt"
    hClose handle
    act $ \xs -> modifyMVar_ count $ \i -> do
        appendFile file $ unlines xs
        return $! i + length xs
    i <- readMVar count
    return (i, file)
