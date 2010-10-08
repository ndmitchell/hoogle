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

import CmdLine.Type
import General.Code
import Text.ParserCombinators.Parsec(ParseError, sourceColumn, errorPos)
import CmdLine.Type
import General.Web
import System.Console.CmdArgs
import Hoogle.Query.All


---------------------------------------------------------------------
-- CMDLINE EXPANSION

cmdLineExpand :: CmdLine -> IO CmdLine
cmdLineExpand x@Search{} = return $ x{queryText = s, queryParsed = f $ parseQuery s}
    where s = unwords $ queryChunks x
          f (Left x) = Left (sourceColumn (errorPos x) - 1, show x)
          f (Right x) = Right x


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
cmdLineWeb args = cmdLineExpand $ blankSearch{web=True,start=askInt "start",count=askInt "count",webmode=ask "mode"}
    where ask x = lookup x args
          askInt x = readMay =<< ask x






{-





queryText :: String                -- ^ The string the user entered, @\"\"@ for no string
query :: NoData {Either ParseError Query   -- ^ The actual query
queryBadFlags :: [String]          -- ^ The bad flags




data CmdQuery = CmdQuery {
    queryWeb :: Bool, -- ^ Are you operating from the web (via CGI)
    queryText :: String, -- ^ The string the user entered, @\"\"@ for no string
    query :: Either ParseError Query, -- ^ The actual query
    queryFlags :: [CmdFlag], -- ^ The flags from the query
    queryBadFlags :: [String] -- ^ The bad flags
    } deriving Show


-- | Left (query text, error message), null query text = no query given
--   Right (query, command line flags, unparsable/unsupported flags)
cmdQuery :: IO CmdQuery
cmdQuery = do
    r <- cgiArgs
    case r of
        Just y -> cmdQueryCGI y
        Nothing -> cmdQueryArgs =<< getArgs


cmdQueryCGI :: [(String,String)] -> IO CmdQuery
cmdQueryCGI [("",x)] = cmdQueryCGI [("q",x)]
cmdQueryCGI xs = do
    (flags1,bad1) <- flagsWebArgs notq
    case parseQuery str of
        Left err -> return $ CmdQuery True str (Left err) flags1 bad1
        Right res -> do
            (flags2,bad2) <- flagsWebQuery $ queryArgs res
            return $ CmdQuery True str (Right res) (flags2++flags1) (bad2++bad1)
    where
        (q,notq) = partition ((`elem` ["q","hoogle"]) . fst) xs
        str = unwords $ map snd q


-- TODO: Checking for web and debug specifically are both hacks
--       Should parse any arguments it can separately
cmdQueryArgs :: [String] -> IO CmdQuery
cmdQueryArgs xs = case parseCmdLineQuery xs of
    Left err -> return $ CmdQuery (hasFlag ["w","web"]) orig (Left err) [Debug | hasFlag ["debug"]] []
    Right res -> do
        (flags,bad) <- flagsCmdLine $ queryArgs res
        return $ CmdQuery (Web `elem` flags) orig (Right res) flags bad
    where orig = unwords $ map quote xs
          hasFlag names = or [(a++b) `elem` xs | a <- ["/","--"], b <- names]


quote :: String -> String
quote x | any isSpace x = "\"" ++ x ++ "\""
        | otherwise = x


queryArgs :: Query -> [(String,String)]
queryArgs q = [(a,b) | Flag a b <- flags q]
-}
