{- |
    Parse the CGI arguments
-}


module Web.Query(webQuery) where

import General.TextUtil
import System.Environment
import Data.Maybe
import Data.Char
import Control.Monad
import Numeric
import Data.List



-- | Return the q parameter, if there is any CGI variable
webQuery :: IO (Maybe String)
webQuery = do v <- var
              return $ case v of
                  Nothing -> Nothing
                  Just x -> Just $ f x
    where
        var = catch (liftM Just $ getEnv "QUERY_STRING")
                    (const $ return Nothing)

        f x | '=' `notElem` x = f ("q=" ++ x)
            | otherwise = fromMaybe "" (lookup "q" $ parseArgs x)



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
