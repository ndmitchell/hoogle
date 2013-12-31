
module Recipe.Warning(
    resetWarnings, putWarning, recapWarnings
    ) where

import Control.Concurrent
import System.IO.Unsafe


{-# NOINLINE warnings #-}
warnings :: MVar [String]
warnings = unsafePerformIO $ newMVar []

putWarning :: String -> IO ()
putWarning x = do
    putStrLn x
    modifyMVar_ warnings $ return . (x:)

recapWarnings :: IO ()
recapWarnings = do
    xs <- readMVar warnings
    mapM_ putStrLn $ reverse xs

resetWarnings :: IO ()
resetWarnings = modifyMVar_ warnings $ const $ return []
