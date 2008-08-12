
module Util(
    module Util,
    module System.Directory,
    module Control.Exception,
    module Data.List,
    module Data.Char,
    module Control.Monad,
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
import Control.Monad
import Control.Exception


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
    h <- openBinaryFile file WriteMode
    hPutStr h x
    hClose h

rep from to x = if x == from then to else x


