
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
searchAll databases query = getResults databases query


-- should be possible to fast-path certain searches, currently not done
searchRange :: (Int,Int) -> [DataBase] -> Query -> [Result]
searchRange (from,to) databases query =
        take (1 + to-from) $ drop from res
    where
        res = getResults databases query


getResults :: [DataBase] -> Query -> [Result]
getResults databases query = orderResults $ filterResults query res
    where
        res = if not (null $ names query) then performTextSearch databases (names query)
              else if isJust (typeSig query) then performTypeSearch databases (fromJust $ typeSig query)
              else error "Search.getResults: Doing a blank search!"


-- | Perform a text query
performTextSearch :: [DataBase] -> [String] -> [Result]
performTextSearch databases (query:_) = concatMap f databases
    where
        f db = [Result e (entryParents db e) [v] [TextScore s] | (e,v,s) <- searchText db query]


performTypeSearch :: [DataBase] -> TypeSig -> [Result]
performTypeSearch databases query = [] -- TODO: concatMap (`searchType` query) databases
