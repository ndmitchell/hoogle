
module Recipe.Type(
    CmdLine(..), Name,
    listing, version,
    resetWarnings, putWarning, recapWarnings
    ) where

import CmdLine.All
import Control.Concurrent
import System.IO.Unsafe
import System.FilePath
import General.Base
import General.System


type Name = String

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
    putStrLn x
    modifyMVar_ warnings $ return . (x:)

recapWarnings :: IO ()
recapWarnings = do
    xs <- readMVar warnings
    mapM_ putStrLn $ reverse xs

resetWarnings :: IO ()
resetWarnings = modifyMVar_ warnings $ const $ return []
