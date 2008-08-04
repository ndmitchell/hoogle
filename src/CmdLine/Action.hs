
module CmdLine.Action(actionCmdLine) where

import CmdLine.Flag
import CmdLine.Query
import CmdLine.Search
import CmdLine.Files
import CmdLine.Test
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
    exitMessage $ "Unrecognised or malformed flags:":
                  map ("  "++) (queryBadFlags q) ++
                  ["For details on correct flags pass --help"]


actionCmdLine q | Version `elem` queryFlags q = putStr $ unlines
    ["Hoogle - (C) Neil Mitchell 2004-2008, University of York, UK"
    ,"Version 4.0 pre"]


actionCmdLine q | Help `elem` queryFlags q =
    putStr $ "Go to the website for full help, http://haskell.org/hoogle/\n" ++
             "\n" ++
             "Flag reference:\n" ++
             flagsHelp


actionCmdLine q | Test `elem` queryFlags q = test


actionCmdLine q | TestFile{} `elemEnum` queryFlags q =
    mapM_ (actionTest q) [x | TestFile x <- queryFlags q]


actionCmdLine q | Rank{} `elemEnum` queryFlags q =
    mapM_ rank [x | Rank x <- queryFlags q]


actionCmdLine q | Convert{} `elemEnum` queryFlags q = do
    mapM_ (actionConvert q) [x | Convert x <- queryFlags q]


actionCmdLine q | Dump{} `elemEnum` queryFlags q = do
    dbs <- getDataBaseFiles (queryFlags q) (fromRight $ query q)
    mapM_ (actionDump q) dbs


actionCmdLine q | not $ usefulQuery $ fromRight $ query q = do
    exitMessage ["No query entered"
                ,"Try --help for command line options"]


actionCmdLine q = actionSearch (queryFlags q) (fromRight $ query q)


---------------------------------------------------------------------
-- SPECIFIC ACTIONS

actionDump :: CmdQuery -> FilePath -> IO ()
actionDump q file = do
    let part = head [x | Dump x <- queryFlags q]
    d <- loadDataBase file
    putStrLn $ "File: " ++ file
    putStr $ showDataBase part d


actionConvert :: CmdQuery -> FilePath -> IO FilePath
actionConvert q infile = do
    let outfile = headDef (replaceExtension infile "hoo") [x | Output x <- queryFlags q]
    putStrLn $ "Converting " ++ infile
    convert (Debug `elem` queryFlags q) infile outfile
    putStrLn $ "Written " ++ outfile
    
    when (Dump{} `elemEnum` queryFlags q) $ do
        putStrLn ""
        actionDump q outfile
    return outfile


actionTest :: CmdQuery -> FilePath -> IO ()
actionTest q infile = do
    outfile <- actionConvert q infile
    testFile infile outfile
