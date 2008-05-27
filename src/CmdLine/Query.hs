
module CmdLine.Query(cmdQuery) where

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import Text.ParserCombinators.Parsec

import CmdLine.Flag
import General.CGI
import Hoogle.Query.All


-- | Left (query text, error message), null query text = no query given
--   Right (query, command line flags, unparsable/unsupported flags)
cmdQuery :: IO (Either (String,ParseError) (Query, [CmdFlag], [String]))
cmdQuery = do
    r <- cgiArgs
    case r of
        Just y -> return $ cmdQueryCGI y
        Nothing -> liftM cmdQueryArgs getArgs


cmdQueryCGI :: [(String,String)] -> Either (String,ParseError) (Query, [CmdFlag], [String])
cmdQueryCGI [("",x)] = cmdQueryCGI [("q",x)]
cmdQueryCGI xs = case parseQuery str of
    Left err -> Left (str,err)
    Right res -> let (a1,b1) = flagsWebQuery $ queryArgs res
                     (a2,b2) = flagsWebArgs notq
                 in Right (res, Web : delete Web (a1++a2), b1++b2)
    where
        (q,notq) = partition ((==) "q" . fst) xs
        str = unwords $ map snd q


cmdQueryArgs :: [String] -> Either (String,ParseError) (Query, [CmdFlag], [String])
cmdQueryArgs xs = case parseCmdLineQuery xs of
    Left err -> Left (unwords $ map quote xs, err)
    Right res -> let (a,b) = flagsCmdLine $ queryArgs res
                 in Right (res, a, b)


quote :: String -> String
quote x | any isSpace x = "\"" ++ x ++ "\""
        | otherwise = x


queryArgs :: Query -> [(String,String)]
queryArgs q = [(a,b) | Flag a b <- flags q]
