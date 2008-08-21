
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


depends :: FilePath -> [FilePath] -> IO () -> IO ()
depends x deps act = do
    b <- doesFileExist x
    if not b then act
     else if null deps then return ()
     else do
        xt <- getModificationTime x
        dt <- liftM maximum $ mapM getModificationTime deps
        when (xt < dt) act
