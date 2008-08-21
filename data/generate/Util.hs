
module Util(
    module Util,
    module System.Directory,
    module Control.Exception,
    module Data.List,
    module Data.Char,
    module Data.Maybe,
    module Control.Monad,
    module Numeric,
    module System.Environment,
    module System.FilePath
    ) where

import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Exception
import Numeric


---------------------------------------------------------------------
-- Pure

isSubstrOf x y = any (x `isPrefixOf`) (tails y)

rep from to x = if x == from then to else x

trim = reverse . ltrim . reverse . ltrim

ltrim = dropWhile isSpace


---------------------------------------------------------------------
-- System.IO

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

writeBinaryFile file x = do
    h <- openBinaryFile file WriteMode
    hPutStr h x
    hClose h


---------------------------------------------------------------------
-- System.Make

depends :: FilePath -> [FilePath] -> IO () -> IO ()
depends x deps act = do
    b <- doesFileExist x
    if not b then act
     else if null deps then return ()
     else do
        xt <- getModificationTime x
        dt <- liftM maximum $ mapM getModificationTime deps
        when (xt < dt) act


---------------------------------------------------------------------
-- Cabal

newtype Cabal = Cabal [String]

readCabal = liftM (Cabal . lines) . readFile

cabalVersion xs = head $ cabalField "version" xs ++ [""]

cabalDepends xs = nub $ filter f $ words $ map (rep ',' ' ') $ unwords $ cabalField "build-depends" xs
    where f x = x /= "" && isAlpha (head x)


cabalField :: String -> Cabal -> [String]
cabalField name (Cabal xs) = f xs
    where
        f (x:xs) | (name ++ ":") `isPrefixOf` map toLower x2 =
                [x4 | x4 /= []] ++ map trim ys ++ f zs
            where
                x4 = trim x3
                x3 = drop (length name + 1) x2
                (spc,x2) = span isSpace x
                (ys,zs) = span ((> length spc) . length . takeWhile isSpace) xs
        f (x:xs) = f xs
        f [] = []


---------------------------------------------------------------------
-- Hoogle.TextBase

-- filter '\r' because of haddock/cabal interactions going weird..
readTextBase :: FilePath -> IO [String]
readTextBase = liftM (lines . filter (/= '\r')) . readFile
