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
import System.Environment
import System.Exit
import System.FilePath


main = do
    xs <- getArgs
    case xs of
        ["sdist"] -> sdist
        ["linecount"] -> wc
        _ -> error $ "Unknown arguments, expected one of: sdist linecount"


---------------------------------------------------------------------
-- SDIST

sdist = do
    sanityCheck
    system_ "cabal build"
    x <- getCurrentDirectory
    bracket_
        (setCurrentDirectory "data/generate")
        (setCurrentDirectory x)
        (system_ "run.bat")
    sanityCheck
    system_ "cabal install --global"
    system_ "cabal sdist"



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


---------------------------------------------------------------------
-- LINECOUNT

wc = do
    src <- liftM (map (dropWhile isSpace) . lines) $ readFile "hoogle.cabal"
    let files = sort $ (:) "Main" $ takeWhile (/= "") $ drop 1 $ dropWhile (/= "other-modules:") src
        lenfiles = maximum $ map length files

    let out x n = do let s = show n
                     putStrLn $ x ++ replicate (8 + lenfiles - length x - length s) ' ' ++ s

    let f x = do sz <- size x
                 out x sz
                 return sz

    xs <- mapM f files
    out "Total" (sum xs)
    

size modu = do
    let file = "src" </> map (\x -> if x == '.' then '/' else x) modu <.> "hs"
    src <- readFile file
    return $ length $ lines src

---------------------------------------------------------------------
-- UTIL

system_ x = do
    putStrLn $ "Running " ++ x
    r <- system x
    when (r /= ExitSuccess) $ error "System command failed"

