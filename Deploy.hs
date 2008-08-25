{-|
    Helper code for deployment of releases
    sdist + sanity checks + database generation
-}

module Main(main) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath


cmds = let (*) = (,) in
       ["sdist" * sdist
       ,"sanity" * sanity
       ,"linecount" * linecount
       ,"web" * web
       ]


main = do
    xs <- getArgs
    case flip lookup cmds =<< listToMaybe xs of
        Nothing -> error $ "Expected one of: " ++ unwords (map fst cmds)
        Just act -> act


---------------------------------------------------------------------
-- SDIST

sdist = do
    sanity
    databases
    sanity
    system_ "cabal install --global"
    system_ "cabal sdist"


databases = do
    system_ "cabal build"
    withDirectory "data/generate" $
        system_ "run.bat"


---------------------------------------------------------------------
-- SANITY CHECK

sanity = do
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
-- SDIST

web = do
    createDirectoryIfMissing True "dist/web/res"
    databases
    system_ "ssh ndm@venice.cs.york.ac.uk -m misc/build-york.sh"
    system_ "scp ndm@venice.cs.york.ac.uk:/tmp/ndm/hoogle/dist/build/hoogle/hoogle dist/web/index.cgi"
    copyFiles "database" "dist/web/res" ["hoo"]
    copyFiles "src/res" "dist/web/res" ["js","css","png","xml"]
    withDirectory "dist/web" $ system_ "tar -cf ../web.tar *"
    system_ "gzip dist/web.tar --force"
    system_ "scp -r dist/web.tar.gz ndm@haskell.org:/haskell/hoogle/release.tar.gz"
    system_ "ssh ndm@haskell.org -m misc/build-haskell.sh"


---------------------------------------------------------------------
-- LINECOUNT

linecount = do
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


copyFiles from to exts = do
    xs <- getDirectoryContents from
    sequence [copyFile (from </> x) (to </> x) | x <- xs
             ,takeExtension x `elem` map ('.':) exts]


withDirectory dir act = do
    x <- getCurrentDirectory
    bracket_
        (setCurrentDirectory dir)
        (setCurrentDirectory x)
        act
