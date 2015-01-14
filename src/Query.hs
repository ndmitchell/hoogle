
module Query(parseQuery, parseRestrict) where

import Data.List
import Data.Tuple.Extra
import Language.Haskell.Exts
import Type

parseQuery :: String -> Query
parseQuery x | "::":xs <- names = Query cat [] (Just $ fromParseResult $ parseType $ unwords xs)
             | otherwise = Query cat names Nothing
    where
        (cat,names) = first (map parseRestrict) $ partition (\x -> not (":" `isPrefixOf` x) && ':' `elem` x) $ words x


parseRestrict :: String -> Restrict
parseRestrict ('+':xs) = parseRestrict xs
parseRestrict ('-':xs) = let Restrict _ a b = parseRestrict xs in Restrict False a b
parseRestrict xs = let (a,_:b) = break (== ':') xs in Restrict True a b
