
module ParseQuery(parseQuery) where

import Data.List
import Data.Tuple.Extra
import Language.Haskell.Exts
import Type

parseQuery :: String -> Query
parseQuery x | "::":xs <- names = Query cat [] (Just $ fromParseResult $ parseType $ unwords xs)
             | otherwise = Query cat names Nothing
    where
        (cat,names) = first (map f) $ partition (\x -> not (":" `isPrefixOf` x) && ':' `elem` x) $ words x

        f ('+':xs) = f xs
        f ('-':xs) = let QTag _ a b = f xs in QTag False a b
        f xs = let (a,_:b) = break (== ':') xs in QTag True a b

