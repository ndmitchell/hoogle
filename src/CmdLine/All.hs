{-|
    Parse a query, that may have come from either a CGI variable
    or the command line arguments.

    Need to return the following pieces of information:

    * Was there a query, or was nothing entered

    * Are you wanting to operate in Web mode or Command Line mode. Adding a
    Web parameter to Command Line gives you Web mode.

    * Which flags were specified, and which were erroneous.
-}
module CmdLine.All(
    cmdLine, cmdLineWeb, CmdLine(..), isWebCmdLine
    ) where

import General.Code
import Text.ParserCombinators.Parsec(sourceColumn, errorPos)
import CmdLine.Type
import General.Web
import System.Console.CmdArgs
import Hoogle.Query.All
import Paths_hoogle


---------------------------------------------------------------------
-- CMDLINE EXPANSION

cmdLineExpand :: CmdLine -> IO CmdLine
cmdLineExpand x@Search{} = do
    db <- expandDatabases $ databases x
    return $ x{queryText = s, queryParsed = f $ parseQuery s, databases = db}
    where s = unwords $ queryChunks x
          f (Left x) = Left (sourceColumn (errorPos x) - 1, show x)
          f (Right x) = Right x

cmdLineExpand x@Server{} = do
    db <- expandDatabases $ databases x
    res <- if null $ resources x then fmap (</> "resources") getDataDir else return $ resources x
    return $ x{databases=db, resources=res}

cmdLineExpand x = return x


expandDatabases x = do
    d <- getDataDir
    return $ x ++ [d </> "databases"]


---------------------------------------------------------------------
-- QUERY CONVERSION

cmdLine :: IO CmdLine
cmdLine = do
    r <- cgiArgs
    case r of
        Just y -> cmdLineWeb y
        Nothing -> cmdLineArgs


cmdLineArgs :: IO CmdLine
cmdLineArgs = cmdLineExpand =<< cmdArgsRun cmdLineMode


cmdLineWeb :: [(String,String)] -> IO CmdLine
cmdLineWeb args = cmdLineExpand $ blankSearch{web=True,
        start=askInt ["start"], count=askInt ["count"], webmode=ask ["mode"],
        queryChunks = maybeToList $ ask ["q","hoogle"]}
    where ask x = listToMaybe [b | (a,b) <- args, a `elem` x]
          askInt x = readMay =<< ask x
