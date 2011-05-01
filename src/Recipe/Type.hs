
module Recipe.Type(
    CmdLine(..), Name, noDeps, safeEncoding,
    keywords, platform, cabals, inputs, listing, version,
    resetWarnings, putWarning, recapWarnings,
    outStr, outStrLn
    ) where

import CmdLine.All
import Control.Concurrent
import System.IO.Unsafe
import General.Base
import General.System


type Name = String


noDeps :: [Name] -> IO ()
noDeps [] = return ()
noDeps xs = error "Internal error: package with no dependencies had dependencies"


-- | Lots of things go slightly wrong if you use characters > 127 in places, this just replaces them with ?
safeEncoding :: String -> String
safeEncoding = map (\x -> if x <= '\0' || x > '\127' then '?' else x)


---------------------------------------------------------------------
-- DOWNLOADED INFORMATION

keywords = "download/keyword.txt"
platform = "download/haskell-platform.cabal"
cabals = "download/hackage-cabal"
inputs = "download/hackage-hoogle"

listing :: FilePath -> IO [Name]
listing dir = do
    xs <- getDirectoryContents dir
    return $ sortBy (comparing $ map toLower) $ filter (`notElem` [".","..","preferred-versions"]) xs

version :: FilePath -> Name -> IO String
version dir x = do
    ys <- getDirectoryContents $ dir </> x
    when (null ys) $ error $ "Couldn't find version for " ++ x ++ " in " ++ dir
    let f = map (read :: String -> Int) . words . map (\x -> if x == '.' then ' ' else x)
    return $ maximumBy (comparing f) $ filter (all (not . isAlpha)) ys


---------------------------------------------------------------------
-- WARNING MESSAGES

{-# NOINLINE warnings #-}
warnings :: MVar [String]
warnings = unsafePerformIO $ newMVar []

putWarning :: String -> IO ()
putWarning x = do
    outStrLn x
    modifyMVar_ warnings $ return . (x:)

recapWarnings :: IO ()
recapWarnings = do
    xs <- readMVar warnings
    mapM_ outStrLn $ reverse xs

resetWarnings :: IO ()
resetWarnings = modifyMVar_ warnings $ const $ return []


outputLock :: MVar ()
outputLock = unsafePerformIO $ newMVar ()

outStr, outStrLn :: String -> IO ()
outStr x = withMVar outputLock $ \_ -> do putStr x; hFlush stdout
outStrLn x = outStr $ x ++ "\n"
