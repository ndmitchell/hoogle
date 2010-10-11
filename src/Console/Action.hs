
module Console.Action(action) where

import CmdLine.All
import Console.Search
import Console.Files
import Console.Test
import General.Code
import Hoogle.All
import Test.All


action :: CmdLine -> IO ()

action Search{queryText = text, queryParsed = Left (pos,err)} =
    exitMessage ["Parse error:", "  " ++ text
                ,replicate pos ' ' ++ "^"
                ,err]


action (Test files) = do
    test
    mapM_ (error "todo - test") files


action (Rank file) = rank file


action (Convert from to) = error "todo - convert" -- actionConvert from to


action Combine{} = error "todo - combine" {- | Combine{} `elemEnum` queryFlags q = do
    -- TODO: Work around a bug in CmdLine.Flags, try /merge=t1;t2
    --       and you get [t1,t1,t2,t2]
    let files = nub [x | Combine x <- queryFlags q]
        outfile = headDef "default.hoo" [x | Output x <- queryFlags q]
    putStrLn $ "Combining " ++ show (length files) ++ " databases"
    combine files outfile
    when (Dump{} `elemEnum` queryFlags q) $ do
        putStrLn ""
        actionDump q outfile
-}

action Dump{} = error "todo - dump" {- do
    dbs <- getDataBaseFiles (queryFlags q) (fromRight $ query q)
    mapM_ (actionDump q) dbs -}


action q@Search{} | not $ usefulQuery $ fromRight $ queryParsed q =
    exitMessage ["No query entered"
                ,"Try --help for command line options"]


action q = actionSearch q (fromRight $ queryParsed q)


---------------------------------------------------------------------
-- SPECIFIC ACTIONS

{-
actionDump :: CmdLine -> FilePath -> IO ()
actionDump q file = do
    let part = head [x | Dump x <- queryFlags q]
    d <- loadDataBase file
    putStrLn $ "File: " ++ file
    putStr $ showDataBase part d

actionConvert :: FilePath -> FilePath -> IO ()
actionConvert infile outfile = do
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
-}
