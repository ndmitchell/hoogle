
module Console.All(action) where

import CmdLine.All
import Recipe.All
import Console.Log
import Console.Search
import Console.Test
import Console.Rank
import General.Base
import General.System
import Hoogle


action :: CmdLine -> IO ()

action x@Search{repeat_=i} | i /= 1 = replicateM_ i $ action x{repeat_=1}

action x@Search{queryParsed = Left err} =
    exitMessage ["Parse error:", "  " ++ showTag (parseInput err)
                ,replicate (columnNo err) ' ' ++ " ^"
                ,errorMessage err]
    where showTag = if color x then showTagANSI else showTagText


action (Test files _) = do
    testPrepare
    fails <- fmap sum $ mapM (testFile action) files
    if fails == 0
        then putStrLn "Tests passed"
        else putStrLn $ "TEST FAILURES (" ++ show fails ++ ")"

action (Rank file) = rank file

action x@Data{} = recipes x

action (Log files) = logFiles files

action (Convert from to) = do
    to <- return $ if null to then replaceExtension from "hoo" else to
    when (any isUpper $ takeBaseName to) $ putStrLn $ "Warning: Hoogle databases should be all lower case, " ++ takeBaseName to
    putStrLn $ "Converting " ++ from
    src <- readFileUtf8 from
    let (err,db) = createDatabase Haskell [] src
    unless (null err) $ putStr $ unlines $ "Warning: parse errors" : map show err
    saveDatabase to db
    putStrLn $ "Written " ++ to

action (Combine from to) = do
    putStrLn $ "Combining " ++ show (length from) ++ " databases"
    xs <- mapM loadDatabase from
    saveDatabase to $ mconcat xs

action (Dump file sections) = do
    d <- loadDatabase file
    putStrLn $ "File: " ++ file
    putStr $ showDatabase d $ if null sections then Nothing else Just sections

action q@Search{} | fromRight (queryParsed q) == mempty =
    exitMessage ["No query entered"
                ,"Try --help for command line options"]

action q@Search{} = actionSearch q (fromRight $ queryParsed q)

