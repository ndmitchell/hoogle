{-|
    Parse a query, that may have come from either a CGI variable
    or the command line arguments.

    Need to return the following pieces of information:
    
    * Was there a query, or was nothing entered
    
    * Are you wanting to operate in Web mode or Command Line mode. Adding a
    Web parameter to Command Line gives you Web mode.

    * Which flags were specified, and which were erroneous.
-}

module CmdLine.Query(cmdQuery, CmdQuery(..)) where

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import Text.ParserCombinators.Parsec

import CmdLine.Flag
import General.CGI
import Hoogle.Query.All


data CmdQuery = CmdQuery {
    queryWeb :: Bool, -- ^ Are you operating from the web (via CGI)
    queryText :: String, -- ^ The string the user entered, @""@ for no string
    query :: Either ParseError Query, -- ^ The actual query
    queryFlags :: [CmdFlag], -- ^ The flags from the query
    queryBadFlags :: [String] -- ^ The bad flags
    }


-- | Left (query text, error message), null query text = no query given
--   Right (query, command line flags, unparsable/unsupported flags)
cmdQuery :: IO CmdQuery
cmdQuery = do
    r <- cgiArgs
    case r of
        Just y -> return $ cmdQueryCGI y
        Nothing -> liftM cmdQueryArgs getArgs


cmdQueryCGI :: [(String,String)] -> CmdQuery
cmdQueryCGI [("",x)] = cmdQueryCGI [("q",x)]
cmdQueryCGI xs = case parseQuery str of
    Left err -> CmdQuery True str (Left err) flags1 bad1
    Right res -> let (flags2,bad2) = flagsWebQuery $ queryArgs res
                 in CmdQuery True str (Right res) (flags2++flags1) (bad2++bad1)
    where
        (q,notq) = partition ((==) "q" . fst) xs
        (flags1,bad1) = flagsWebArgs notq
        str = unwords $ map snd q


cmdQueryArgs :: [String] -> CmdQuery
cmdQueryArgs xs = case parseCmdLineQuery xs of
    Left err -> CmdQuery False orig (Left err) [] []
    Right res -> let (flags,bad) = flagsCmdLine $ queryArgs res
                 in CmdQuery (Web `elem` flags) orig (Right res) flags bad
    where orig = unwords $ map quote xs


quote :: String -> String
quote x | any isSpace x = "\"" ++ x ++ "\""
        | otherwise = x


queryArgs :: Query -> [(String,String)]
queryArgs q = [(a,b) | Flag a b <- flags q]
