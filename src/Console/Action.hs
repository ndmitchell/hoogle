
module Console.Action(action) where

import CmdLine.All
import Console.Search
import Console.Files
import Console.Test
import General.Code
import Hoogle.All
import Test.All
import Text.ParserCombinators.Parsec

import Paths_hoogle(version)
import Data.Version(showVersion)


action :: CmdQuery -> IO ()

action CmdQuery{queryText = text, query = Left err} =
    exitMessage ["Parse error:", "  " ++ text
                ,replicate (sourceColumn (errorPos err) + 1) ' ' ++ "^"
                ,show err]


action q | not $ null $ queryBadFlags q =
    exitMessage $ "Unrecognised or malformed flags:":
                  map ("  "++) (queryBadFlags q) ++
                  ["For details on correct flags pass --help"]


action q | Version `elem` queryFlags q = putStr $ unlines
    ["Hoogle - (C) Neil Mitchell 2004-2010"
    ,"Version " ++ showVersion version]


action q | Help `elem` queryFlags q =
    putStr $ "Go to the website for full help, http://haskell.org/hoogle/\n" ++
             "\n" ++
             "Flag reference:\n" ++
             flagsHelp


action q | Test `elem` queryFlags q = test


action q | TestFile{} `elemEnum` queryFlags q =
    mapM_ (actionTest q) [x | TestFile x <- queryFlags q]


action q | Rank{} `elemEnum` queryFlags q =
    mapM_ rank [x | Rank x <- queryFlags q]


action q | Convert{} `elemEnum` queryFlags q =
    mapM_ (actionConvert q) [x | Convert x <- queryFlags q]


action q | Combine{} `elemEnum` queryFlags q = do
    -- TODO: Work around a bug in CmdLine.Flags, try /merge=t1;t2
    --       and you get [t1,t1,t2,t2]
    let files = nub [x | Combine x <- queryFlags q]
        outfile = headDef "default.hoo" [x | Output x <- queryFlags q]
    putStrLn $ "Combining " ++ show (length files) ++ " databases"
    combine files outfile
    when (Dump{} `elemEnum` queryFlags q) $ do
        putStrLn ""
        actionDump q outfile


action q | Dump{} `elemEnum` queryFlags q = do
    dbs <- getDataBaseFiles (queryFlags q) (fromRight $ query q)
    mapM_ (actionDump q) dbs


action q | not $ usefulQuery $ fromRight $ query q =
    exitMessage ["No query entered"
                ,"Try --help for command line options"]


action q = actionSearch (queryFlags q) (fromRight $ query q)


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
    deps <- getDataBaseFilesNoDefault (queryFlags q) (fromRight $ query q)
    convert (Debug `elem` queryFlags q) deps infile outfile
    putStrLn $ "Written " ++ outfile
    
    when (Dump{} `elemEnum` queryFlags q) $ do
        putStrLn ""
        actionDump q outfile
    return outfile


actionTest :: CmdQuery -> FilePath -> IO ()
actionTest q infile = do
    outfile <- actionConvert q{queryFlags = Debug : queryFlags q} infile
    testFile infile outfile
