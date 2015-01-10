
module ParseQuery(parseQuery) where

import Type

parseQuery :: String -> Query
parseQuery x = Query [] (words x) Nothing
