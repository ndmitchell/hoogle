
module Main where

import Web.All
import CmdLine.All
import Hoogle.All
import General.All

import Control.Monad
import Data.Maybe
import Data.List
import System.FilePath
import System.Directory


data Origin = Web
            | CmdLine


getQuery :: IO (Origin, String)
getQuery = do web <- webQuery
              case web of
                  Just x -> return (Web, x)
                  Nothing -> liftM ((,) CmdLine) cmdQuery 

main = do
    (origin,query) <- getQuery
    case parseQuery query of
        Left err -> putStr $ cmdParseError query err
        Right q -> exec origin q


hasFlag :: Query -> [String] -> Bool
hasFlag q s = isJust $ getFlag q s


getFlag :: Query -> [String] -> Maybe String
getFlag query s = listToMaybe [extra | Flag name extra <- flags query, name `elem` s]

checkFlags :: Query -> [String] -> IO ()
checkFlags query s = when (not $ null extra) $
        putStrLn $ "Warning, unrecognised flag" ++ ['s'|length extra > 1] ++ " (ignored): " ++
                   concat (intersperse ", " extra)
    where extra = [name | Flag name _ <- flags query] \\ s


versionMsg = unlines
    ["HOOGLE - (C) Neil Mitchell 2004-2007, University of York, UK"
    ,"Version 4.0 pre"
    ]

helpMsg = unlines
    ["Go to the website for help, http://haskell.org/hoogle/"]



fVersion = ["v","ver","version"]
fHelp = ["h","help"]
fConvert = ["conv","convert"]
fOutput = ["o","out","output"]
fDatabase = ["db","data","database"]
fColor = ["c","col","color","colour"]


exec :: Origin -> Query -> IO ()

exec CmdLine q | hasFlag q fVersion = putStr versionMsg

exec CmdLine q | hasFlag q fHelp = putStr helpMsg

exec CmdLine q | hasFlag q fConvert = do
    let input  = fromMaybe "" (getFlag q fConvert)
        output = fromMaybe (replaceExtension input "hoo") (getFlag q fOutput)
    checkFlags q (fConvert ++ fOutput)
    exist <- doesFileExist input
    if not exist
        then putStrLn $ "Convert, input file not found: " ++ input
        else do
        putStrLn $ "Convert, begining on " ++ input
        response <- newDataBase input output
        if anyError response
            then putStrLn $ "Conversion failed"
            else putStrLn $ "Conversion successful, created: " ++ output
    

exec CmdLine q | not $ usefulQuery q = putStr $ "No query given\n" ++ helpMsg

exec CmdLine q = do
    checkFlags q (fColor ++ fDatabase)
    let file = fromMaybe "base.hoo" (getFlag q fDatabase)
    db <- loadDataBase file
    case db of
        Nothing -> putStrLn $ "Failed to load database, " ++ file
        Just x -> do
            res <- searchAll [x] q
            putStr $ unlines $ map (showTags . renderResult) res

    where
        showTags = if hasFlag q fColor then showTagConsole else showTag
