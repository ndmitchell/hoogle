
module Console.Action(action) where

import CmdLine.All
import Console.Search
import Console.Test
import Console.Rank
import General.Code
import Data.Monoid
import Test.All
import Hoogle


action :: CmdLine -> IO ()

action Search{queryText = text, queryParsed = Left (ParseError _ pos err)} =
    exitMessage ["Parse error:", "  " ++ text
                ,replicate pos ' ' ++ "^"
                ,err]

action (Test files) = do
    test
    mapM_ testFile files

action (Rank file) = rank file

action (Convert from to) = do
    to <- return $ if null to then replaceExtension from "hoo" else to
    putStrLn $ "Converting " ++ from
    src <- readFile from
    let db = case createDatabase [] src of
            Left x -> error $ "Parse error with " ++ from ++ "\n" ++ show x
            Right x -> x
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

action q@Search{} | isBlankQuery $ fromRight $ queryParsed q =
    exitMessage ["No query entered"
                ,"Try --help for command line options"]

action q@Search{} = actionSearch q (fromRight $ queryParsed q)
