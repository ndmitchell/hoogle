
module Recipe.Type(
    CmdLine(..), Name, hoo, noDeps,
    keywords, platform, cabals, haddocks, listing, version,
    resetWarnings, putWarning, recapWarnings,
    outStr, outStrLn,
    Cabal(..), readCabal, readCabalDepends, readCabalField
    ) where

import CmdLine.All
import Control.Concurrent
import System.IO.Unsafe
import General.Base
import General.Util
import General.System


type Name = String

hoo :: Name -> FilePath
hoo x = x <.> "hoo"


noDeps :: [Name] -> IO ()
noDeps [] = return ()
noDeps xs = error "Internal error: package with no dependencies had dependencies"


---------------------------------------------------------------------
-- DOWNLOADED INFORMATION

keywords = "download/keyword.txt"
platform = "download/haskell-platform.cabal"
cabals = "download/hackage-cabal"
haddocks = "download/hackage-haddock"

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


---------------------------------------------------------------------
-- CABAL

data Cabal = Cabal {cabalDepends :: [String], cabalDescription :: [String]}

readCabal :: FilePath -> IO Cabal
readCabal file = do
    src <- fmap lines $ readFile' file
    return $ Cabal (readCabalDepends src) (readCabalField src True "description")


readCabalDepends :: [String] -> [String]
readCabalDepends xs = nub $ map (takeWhile g) $ filter f $ words $ map (rep ',' ' ') $ unwords $ readCabalField xs False "build-depends"
    where f x = x /= "" && isAlpha (head x)
          g x = isAlphaNum x || x `elem` "-_"


readCabalField :: [String] -> Bool -> String -> [String]
readCabalField xs root name = f xs
    where
        f (x:xs) | (name ++ ":") `isPrefixOf` map toLower x2 && (null spc || not root) =
                [x4 | x4 /= []] ++ map (rep "." "" . trim) ys ++ f zs
            where
                x4 = trim x3
                x3 = drop (length name + 1) x2
                (spc,x2) = span isSpace x
                (ys,zs) = span ((> length spc) . length . takeWhile isSpace) xs
        f (x:xs) = f xs
        f [] = []
