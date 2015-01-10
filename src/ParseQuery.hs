
module ParseQuery(parseQuery) where

import Data.List
import Type

parseQuery :: String -> Query
parseQuery x = Query (map f cat) names Nothing
    where
        (cat,names) = partition (':' `elem`) $ words x
        f ('+':xs) = f xs
        f ('-':xs) = let QTag _ a b = f xs in QTag False a b
        f xs = let (a,_:b) = break (== ':') xs in QTag True a b

