{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

{- |
    Parse the CGI arguments
-}


module Web.CGI(cgiArgs, escape, asCgi) where

import Hoogle.TextUtil
import System
import Maybe
import Char
import Numeric
import List


cgiVariable :: IO String
cgiVariable = catch (getEnv "QUERY_STRING")
                    (\ _ -> do x <- getArgs
                               return $ concat $ intersperse " " x)


cgiArgs :: IO [(String, String)]
cgiArgs = do x <- cgiVariable
             let args = if '=' `elem` x then x else "q=" ++ x
             return $ parseArgs args

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


escape :: String -> String
escape (x:xs) | isAlphaNum x = x : escape xs
              | otherwise    = '%' : escapeChar x ++ escape xs
escape [] = []


escapeChar :: Char -> String
escapeChar x = case showHex (ord x) "" of
                  [x] -> ['0',x]
                  x   -> x
