{-# LANGUAGE PatternGuards #-}

{- |
    General web utility functions.
-}

module General.Web(
    Header, headerContentType,
    escapeURL, unescapeURL,
    escapeHTML,
    cgiArgs, cgiResponse,
    httpRequest, httpGetArgs, httpResponse,
    parseHttpQueryArgs
    ) where

import General.TextUtil
import System.Environment
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Numeric
import System.IO


data Header = Header String String

instance Show Header where
    show (Header x y) = x ++ ": " ++ y

headerContentType x = Header "Content-type" x


---------------------------------------------------------------------
-- HTML STUFF

-- | Take a piece of text and escape all the HTML special bits
escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f  x  = [x]


---------------------------------------------------------------------
-- URL STUFF

-- | Take an escape encoded string, and return the original
unescapeURL :: String -> String
unescapeURL ('+':xs) = ' ' : unescapeURL xs
unescapeURL ('%':a:b:xs) | [(v,"")] <- readHex [a,b] = chr v : unescapeURL xs
unescapeURL (x:xs) = x : unescapeURL xs
unescapeURL [] = []


escapeURL :: String -> String
escapeURL (x:xs) | isAlphaNum x = x : escapeURL xs
                 | otherwise = '%' : f (showHex (ord x) "") ++ escapeURL xs
    where f x = ['0' | length x == 1] ++ x
escapeURL [] = []


---------------------------------------------------------------------
-- CGI STUFF

-- The BOA server does not set QUERY_STRING if it would be blank.
-- However, it does always set REQUEST_URI.
cgiVariable :: IO (Maybe String)
cgiVariable = do
    str <- envVariable "QUERY_STRING"
    if isJust str
        then return str
        else liftM (liftM $ const "") $ envVariable "REQUEST_URI"


envVariable :: String -> IO (Maybe String)
envVariable x = catch (liftM Just $ getEnv x) (const $ return Nothing)


cgiArgs :: IO (Maybe [(String, String)])
cgiArgs = do
    x <- cgiVariable
    return $ case x of
        Nothing -> Nothing
        Just y -> Just $ parseArgs $ ['=' | '=' `notElem` y] ++ y


parseArgs :: String -> [(String, String)]
parseArgs xs = mapMaybe (f . splitPair "=") $ splitList "&" xs
    where f Nothing = Nothing
          f (Just (a,b)) = Just (unescapeURL a, unescapeURL b)


cgiResponse :: [Header] -> String -> IO ()
cgiResponse xs x = putStrLn $ intercalate "\n" $ map show xs ++ ["",x]


---------------------------------------------------------------------
-- HTTP STUFF

parseHttpQueryArgs = parseArgs

httpRequest :: Handle -> IO [String]
httpRequest h = do
    x <- hGetLine h
    if all isSpace x then return [] else do
        xs <- httpRequest h
        return $ x : xs


httpGetArgs :: Handle -> IO (String,[(String,String)])
httpGetArgs h = do
    xs <- httpRequest h
    let url = words (head xs) !! 1
    let (page,args) = fromMaybe (url,"") $ splitPair "?" url
    return (page, parseArgs args)


httpResponse :: Handle -> [Header] -> String -> IO ()
httpResponse h xs x = hPutStr h $ intercalate "\r\n" $ "HTTP/1.1 200 OK" : map show xs ++ ["",x]
