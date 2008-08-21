{-|
    Helper code for deployment of releases
    sdist + sanity checks + database generation
-}

module Main(main) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath


main = do
    sanityCheck
    system_ "cabal install --global"
    x <- getCurrentDirectory
    bracket_
        (setCurrentDirectory "data/generate")
        (setCurrentDirectory x)
        (system_ "run.bat")
    system_ "cabal install --global"
    system_ "cabal sdist"


system_ x = do
    putStrLn $ "Running " ++ x
    r <- system x
    when (r /= ExitSuccess) $ error "System command failed"


sanityCheck = do
    src <- liftM (map (dropWhile isSpace) . lines) $ readFile "hoogle.cabal"
    let grab x = takeWhile (/= "") $ drop 1 $ dropWhile (/= x) src

    dbs <- filterM (doesFileExist . (</>) "database") =<< getDirectoryContents "database"
    check (grab "data-files:") dbs

    system_ "ghc -M src/Main -isrc -i."
    deps <- readFile "Makefile"
    length deps `seq` removeFile "Makefile"
    check (grab "other-modules:") (parseMakefile deps \\ ["Main"])

    putStrLn "Sanity check passed"


parseMakefile = nub . concatMap f . concatMap words . lines
    where
        f xs = [map g $ dropExtension $ drop 4 xs | "src/" `isPrefixOf` xs]
        g x = if x == '/' then '.' else x


check left right = do
    let badLeft = left \\ right
        badRight = right \\ left
    when (not $ null badLeft && null badRight) $ do
        print (badLeft,badRight)
        error "Discrepancy detected"
