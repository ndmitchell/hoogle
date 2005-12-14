
module Web.Lambdabot(query) where

import List
import Char

query :: String -> IO (Maybe String)
query x = do d <- readDatabase
             return $ case filter ((==) (prepSearch x) . fst) d of
                (x:xs) -> Just $ formatRes (snd x)
                [] -> Nothing


prepSearch = map toLower . reverse . dropWhile isSpace . reverse . dropWhile isSpace 

formatRes = unwords . map linky . words

linky x | "http://" `isPrefixOf` x = "<a href='" ++ x ++ "'>" ++ x ++ "</a>"
        | otherwise = x


readDatabase :: IO [(String, String)]
readDatabase = do x <- readFile "res/lambdabot.txt"
                  return $ f (lines x)
    where
        f (key:val:xs) = (key,val) : f xs
        f _ = []

