
module Recipe.Warning(
    withWarnings
    ) where

import Control.Concurrent


withWarnings :: (([String] -> IO ()) -> IO ()) -> IO (Int, FilePath)
withWarnings act = do
    count <- newMVar 0
    let file = ".warnings"
    writeFile file ""
    act $ \xs -> modifyMVar_ count $ \i -> do
        appendFile file $ unlines xs
        return $! i + length xs
    i <- readMVar count
    return (i, file)
