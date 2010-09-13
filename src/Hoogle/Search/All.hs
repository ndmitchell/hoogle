
module Hoogle.Search.All(
    Result(..), renderResult,
    searchAll, searchRange
    ) where

import Data.Range

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Result
import Hoogle.Search.Results
import Hoogle.TypeSig.All


-- return all the results
searchAll :: [DataBase] -> Query -> [Result]
searchAll databases query = getResults query databases


-- should be possible to fast-path certain searches, currently not done
searchRange :: Range -> [DataBase] -> Query -> [Result]
searchRange r databases query = listRange r $ getResults query databases


getResults :: Query -> [DataBase] -> [Result]
getResults query = mergeDataBaseResults . map (mergeQueryResults query . f)
    where
        f d = [typeSearch d q | Just q <- [typeSig query]] ++
              map (nameSearch d) (names query)


nameSearch :: DataBase -> String -> [Result]
nameSearch db query = [Result e [v] [TextScore s]
                      | (e,v,s) <- searchName db query]


typeSearch :: DataBase -> TypeSig -> [Result]
typeSearch db query = [Result e v [TypeScore s]
                      | (e,v,s) <- searchType db query]
