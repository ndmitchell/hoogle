
module Main where

import System.Cmd
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.IO

import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Safe


packages = ["base","Cabal","HUnit","QuickCheck","array","bytestring"
           ,"cgi","containers","directory","filepath","haskell-src","mtl"
           ,"network","old-locale","old-time","packedstring","parallel"
           ,"parsec","pretty","process","random","stm","template-haskell"
           ,"time","xhtml"]
           \\
           ["network","cgi"]

keywords = ["|","->","<-","@","!","::","~","_","as","case","class","data"
           ,"default","deriving","do","else","forall","hiding","if","import"
           ,"in","infix","infixl","infixr","instance","let","module","newtype"
           ,"of","qualified","then","type","where"]

prefix = "http://darcs.haskell.org/ghc-6.8/packages/"

main = do
    args <- getArgs
    let rebuild = "skip" `notElem` args
    createDirectoryIfMissing True "grab"
    createDirectoryIfMissing True "result"
    mapM_ (generate rebuild) packages


---------------------------------------------------------------------
-- GENERATE A HOOGLE DATABASE

generate rebuild url = do
    let name = last $ words $ map (\x -> if x == '/' then ' ' else x) url
        dir = "grab" </> name
        exe = dir </> "Setup"
        database = dir </> "dist" </> "doc" </> "html" </> name </> name <.> "txt"
        cabal = dir </> name <.> "cabal.old"
    db <- doesFileExist database

    when (rebuild || not db) $ do
        b <- doesDirectoryExist dir
        if b
            then do system_ $ "darcs pull --all --repodir=" ++ dir
                    system_ $ "darcs rev --all --repodir=" ++ dir
            else system_ $ "darcs get --partial " ++ prefix ++ url ++ "/ --repodir=" ++ dir

        setCurrentDirectory $ "grab" </> name
        fixup name
        system_ $ "ghc -i --make Setup"
        system_ $ "setup configure"
        system_ $ "setup haddock --hoogle"

        setCurrentDirectory "../.."

    (version,depends) <- cabalInfo cabal
    src <- readFile database
    writeFile ("result" </> name <.> "txt") $ unlines $ concatMap (f version depends) $ lines src
    where
        f version depends x
            | x == "module Prelude" = x:map ("keyword "++) keywords
            | "@package" `isPrefixOf` x = x : ["@version " ++ version | version /= ""] ++
                                          ["@depends " ++ d | d <- depends]
            | "@version" `isPrefixOf` x && version /= "" = []
            | otherwise = [x]


cabalInfo file = do
    src <- liftM lines $ readFile file
    let version = headDef "" $ readFields "version" src
        depends = nub $ filter f $ words $ map (rep ',' ' ') $ unwords $ readFields "build-depends" src
        f x = x /= "" && isAlpha (head x)
    return (version,depends)


readFields :: String -> [String] -> [String]
readFields name = concatMap f
    where
        f x | (name ++ ":") `isPrefixOf` map toLower x2 = [x4]
            where
                x4 = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace x3
                x3 = drop (length name + 1) x2
                x2 = dropWhile isSpace x
        f x = []


---------------------------------------------------------------------
-- FIXUP A BUILD

badCabal = ["GHC.Prim"]

fixup name = do
    -- FIX THE SETUP FILE
    removeFile_ "Setup.hs"
    removeFile_ "Setup.lhs"
    writeFile "Setup.hs" "import Distribution.Simple; main = defaultMain"

    -- FIX THE CABAL FILE
    let file = name <.> "cabal"
    copyFile file (file <.> "old")
    x <- readFile' file

    -- trim build-depends as they may not exist on GHC 
    let f x = not $ any (\y -> map toLower y `isSubstrOf` map toLower x) badCabal
    x <- return $ unlines $ filter f $ lines x

    writeBinaryFile file x

    -- INCLUDE FILES
    let incdir = "include"
        n:ame = if "old-" `isPrefixOf` name then drop 4 name else name
        file = incdir </> ("Hs" ++ [toUpper n] ++ ame ++ "Config") <.> "h"
    b <- doesDirectoryExist incdir
    when b $ copyFile "../../Config.h" file


---------------------------------------------------------------------
-- UTILITY FUNCTIONS

system_ x = do
    putStrLn $ "Running: " ++ x
    res <- system x
    when (res /= ExitSuccess) $ do
        putStrLn "Command failed"
        exitFailure

removeFile_ x = do
    b <- doesFileExist x
    when b $ removeFile x

readFile' x = do
    h <- openFile x ReadMode
    s <- hGetContents h
    () <- length s `seq` return ()
    hClose h
    return s

isSubstrOf x y = any (x `isPrefixOf`) (tails y)

writeBinaryFile file x = do
    withBinaryFile file WriteMode (\h -> hPutStr h x)

rep from to x = if x == from then to else x
