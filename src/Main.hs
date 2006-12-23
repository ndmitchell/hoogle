
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
getFlag query s = listToMaybe $ getFlags query s

getFlags :: Query -> [String] -> [String]
getFlags query s = [extra | Flag name extra <- flags query, name `elem` s]


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
fStart = ["s","start"]
fCount = ["n","count","length","len"]


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
        print response
        if anyError response
            then putStrLn $ "Conversion failed"
            else putStrLn $ "Conversion successful, created: " ++ output
    

exec CmdLine q | not $ usefulQuery q = putStr $ "No query given\n" ++ helpMsg

exec CmdLine q = do
    checkFlags q (fColor ++ fDatabase ++ fStart ++ fCount)
    databases <- collectDataBases q
    res <- searcher databases
    putStr $ unlines $ map (showTags . renderResult) res
    where
        showTags = if hasFlag q fColor then showTagConsole else showTag
        
        searcher dbs | isJust start || isJust count
                     = searchRange dbs q (fromMaybe 1 start - 1) (fromMaybe 25 count)
                     | otherwise = searchAll dbs q
        
        start = getPosIntFlag fStart
        count = getPosIntFlag fCount
        
        getPosIntFlag flags =
            case getFlag q flags of
                Nothing -> Nothing
                Just x ->
                    case (reads x :: [(Int,String)]) of
                        [(n,"")] | n > 0 -> Just n
                        _ -> Nothing


{- RULES
For each /db=... flag, it must be either a file (load it) or a folder (look for +packages in it)
If always check the current directory if all /db directives fail
If no /db files and no +packages then default to +base
-}
collectDataBases :: Query -> IO [DataBase]
collectDataBases q = do
    (files,dirs) <- f (getFlags q fDatabase)
    let packs = [x | PlusPackage x <- scope q]
    files2 <- mapM (g (dirs++[""])) (if null packs && null files then ["base"] else packs)
    res <- mapM h (files ++ catMaybes files2)
    return $ catMaybes res
    where
        f :: [FilePath] -> IO ([FilePath],[FilePath])
        f [] = return ([], [])
        f (x:xs) = do
            bfile <- doesFileExist x
            bdir <- doesDirectoryExist x
            if not (bfile || bdir)
                then do
                    putStrLn $ "Warning, database not found: " ++ x
                    f xs
                else do
                    (a,b) <- f xs
                    return ([x|bfile]++a, [x|not bfile]++b)

        -- maybe return an item
        g :: [FilePath] -> String -> IO (Maybe FilePath)
        g (x:xs) y = do
            let file = x </> y <.> "hoo"
            b <- doesFileExist file
            if b then return $ Just file
                 else g xs y
        g [] y = do
            putStrLn $ "Warning, failed to find package " ++ y
            return Nothing

        h file = do
            db <- loadDataBase file
            when (isNothing db) $ putStrLn $ "Failed to load database, " ++ file
            return db
