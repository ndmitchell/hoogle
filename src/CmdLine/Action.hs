
module CmdLine.Action(actionCmdLine) where

import CmdLine.Flag
import CmdLine.Query
import CmdLine.Search
import General.Code
import Hoogle.All
import Test.All
import Text.ParserCombinators.Parsec


actionCmdLine :: CmdQuery -> IO ()

actionCmdLine CmdQuery{queryText = text, query = Left err} = do
    exitMessage ["Parse error:", "  " ++ text
                ,replicate (sourceColumn (errorPos err) + 1) ' ' ++ "^"
                ,show err]


actionCmdLine q | not $ null $ queryBadFlags q = do
    exitMessage ["Unrecognised or malformed flags:"
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
    when (not exist) $
        exitMessage ["Convert, input file not found: " ++ infile]

    putStrLn $ "Converting " ++ infile
    convert (Debug `elem` queryFlags q) infile outfile
    putStrLn $ "Written " ++ outfile


actionCmdLine q | Dump{} `elemEnum` queryFlags q = do
    db <- loadDataBase $ head [x | Dump x <- queryFlags q]
    print db


actionCmdLine q | not $ usefulQuery $ fromRight $ query q = do
    exitMessage ["No query entered"
                ,"Try --help for command line options"]


actionCmdLine q = actionSearch (queryFlags q) (fromRight $ query q)
