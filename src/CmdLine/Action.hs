
module CmdLine.Action(actionCmdLine) where

import CmdLine.Flag
import CmdLine.Query
import Control.Monad
import Data.List
import General.All
import Hoogle.All
import Test.All
import Text.ParserCombinators.Parsec
import Safe
import System.Directory
import System.Exit
import System.FilePath


failMessage :: [String] -> IO ()
failMessage msg = putStr (unlines msg) >> exitFailure


actionCmdLine :: CmdQuery -> IO ()

actionCmdLine CmdQuery{queryText = text, query = Left err} = do
    putStrLn $ msg ++ show err
    exitFailure
    where
        msg = unlines ["Parse error:", "  " ++ text,
                        replicate (sourceColumn (errorPos err) + 1) ' ' ++ "^"]


actionCmdLine q | not $ null $ queryBadFlags q = do
    failMessage ["Unrecognised or malformed flags:"
                ,"  " ++ concat (intersperse ", " $ map show $ queryBadFlags q)
                ,"For details on correct flags pass --help"]


actionCmdLine q | Version `elem` queryFlags q = putStr $ unlines
    ["Hoogle - (C) Neil Mitchell 2004-2008, University of York, UK"
    ,"Version 4.0 pre"]


actionCmdLine q | Help `elem` queryFlags q =
    putStr $ "Go to the website for full help, http://haskell.org/hoogle/\n" ++
             "\n" ++
             "Flag reference:\n" ++
             flagsHelp


actionCmdLine q | Test `elem` queryFlags q = test


actionCmdLine q | Convert{} `elemEnum` queryFlags q = do
    let infile = head [x | Convert x <- queryFlags q]
        outfile = headDef (replaceExtension infile "hoo") [x | Output x <- queryFlags q]

    exist <- doesFileExist infile
    when (not exist) $ failMessage ["Convert, input file not found: " ++ infile]

    putStrLn $ "Converting " ++ infile
    convert infile outfile


actionCmdLine _ = error "todo"
