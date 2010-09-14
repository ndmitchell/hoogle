
{- |
    Parse the CGI arguments
-}


module General.CGI(cgiArgs, escape, asCgi, escapeHTML, escapeAttrib) where

import General.TextUtil
import System.Environment
import Control.Monad
import Data.Maybe
import Data.Char
import Numeric
import Data.List


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
cgiArgs = do x <- cgiVariable
             return $ case x of
                Nothing -> Nothing
                Just y -> Just $ parseArgs $ ['=' | '=' `notElem` y] ++ y


asCgi :: [(String, String)] -> String
asCgi x = concat $ intersperse "&" $ map f x
    where
        f (a,b) = a ++ "=" ++ escape b


parseArgs :: String -> [(String, String)]
parseArgs xs = mapMaybe (parseArg . splitPair "=") $ splitList "&" xs

parseArg Nothing = Nothing
parseArg (Just (a,b)) = Just (unescape a, unescape b)


-- | Take an escape encoded string, and return the original
unescape :: String -> String
unescape ('+':xs) = ' ' : unescape xs
unescape ('%':a:b:xs) = unescapeChar a b : unescape xs
unescape (x:xs) = x : unescape xs
unescape [] = []


-- | Takes two hex digits and returns the char
unescapeChar :: Char -> Char -> Char
unescapeChar a b = chr $ (f a * 16) + f b
    where
        f x | isDigit x = ord x - ord '0'
            | otherwise = ord (toLower x) - ord 'a' + 10


-- | Decide how you want to encode individual characters
--   i.e. upper or lower case
escape :: String -> String
escape (x:xs) | isAlphaNum x = x : escape xs
              | otherwise = '%' : f (showHex (ord x) "") ++ escape xs
    where f x = ['0' | length x == 1] ++ x
escape [] = []


-- | Take a piece of text and escape all the HTML special bits
escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f  x  = [x]


escapeAttrib :: String -> String
escapeAttrib = concatMap f . escapeHTML
    where
        f '\"' = "&quot;"
        f x = [x]
