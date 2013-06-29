
module Hoogle.Search.All(search) where

import Data.List (sortBy)
import Data.Maybe
import Data.Ord (comparing)
import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Results
import Hoogle.Type.All
import Hoogle.Store.All


-- return all the results, lazily
search :: [DataBase] -> Query -> [Result]
search databases query = getResults query databases


getResults :: Query -> [DataBase] -> [Result]
getResults query = sortBy ((if invertResults query then flip else id)
                           $ comparing resultScore) .
                   mergeDataBaseResults . map (mergeQueryResults query . f)
    where
        f d = [ typeSearch d q
              | Just q <- [typeSig query], isNothing (exactSearch query) ] ++
              map (nameSearch d (exactSearch query)) (names query)


nameSearch :: DataBase -> Maybe ItemKind -> String -> [Result]
nameSearch db kind query =
    [ Result (fromOnce e) [v] s
    | (e,v,s) <- (maybe searchName searchExactName kind) db query ]


typeSearch :: DataBase -> TypeSig -> [Result]
typeSearch db query = [Result (fromOnce e) v s
                      | (e,v,s) <- searchType db query]
