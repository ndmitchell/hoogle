
module Hoogle.Search.All(
    Result(..), renderResult,
    searchAll, searchRange
    ) where

import Data.Maybe

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Result
import Hoogle.Search.Results
import Hoogle.TypeSig.All


-- return all the results
searchAll :: [DataBase] -> Query -> [Result]
searchAll databases query = getResults query databases


-- should be possible to fast-path certain searches, currently not done
searchRange :: (Int,Int) -> [DataBase] -> Query -> [Result]
searchRange (from,to) databases query =
        take (1 + to-from) $ drop from res
    where
        res = getResults query databases


getResults :: Query -> [DataBase] -> [Result]
getResults query = mergeDataBaseResults . map (mergeQueryResults query . f)
    where
        f d = [typeSearch d q | Just q <- [typeSig query]] ++
              map (textSearch d) (names query)


-- | Perform a text query
textSearch :: DataBase -> String -> [Result]
textSearch db query = [Result e (entryParents db e) [v] [TextScore s]
                      | (e,v,s) <- searchText db query]


typeSearch :: DataBase -> TypeSig -> [Result]
typeSearch db query = []
