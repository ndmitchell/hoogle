
{- |
    Parse the CGI arguments
-}


module General.CGI(cgiArgs, escape, escapeUpper, escapeLower, asCgi, escapeHTML, escapeAttrib) where

import General.TextUtil
import System.Environment
import Control.Monad
import Data.Maybe
import Data.Char
import Numeric
import Data.List


cgiVariable :: IO (Maybe String)
cgiVariable = catch (liftM Just $ getEnv "QUERY_STRING")
                    (const $ return Nothing)


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
escapeWith :: (Char -> Char) -> String -> String
escapeWith f (x:xs) | isAlphaNum x = x : escapeWith f xs
                    | otherwise    = '%' : escapeCharWith f x ++ escapeWith f xs
escapeWith f [] = []


escapeCharWith :: (Char -> Char) -> Char -> String
escapeCharWith f x = case map f $ showHex (ord x) "" of
                          [x] -> ['0',x]
                          x   -> x

escapeUpper = escapeWith toUpper
escapeLower = escapeWith toLower
escape = escapeLower



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
