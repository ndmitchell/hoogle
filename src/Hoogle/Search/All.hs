
module Hoogle.Search.All(search) where

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Results
import Hoogle.Type.All
import Hoogle.Store.All


-- return all the results, lazily
search :: [DataBase] -> Query -> [Result]
search databases query = getResults query databases


getResults :: Query -> [DataBase] -> [Result]
getResults query = mergeDataBaseResults . map (mergeQueryResults query . f)
    where
        f d = [typeSearch d q | Just q <- [typeSig query]] ++
              map (nameSearch d) (names query)


nameSearch :: DataBase -> String -> [Result]
nameSearch db query = [Result (fromOnce e) [v] s
                      | (e,v,s) <- searchName db query]


typeSearch :: DataBase -> TypeSig -> [Result]
typeSearch db query = [Result (fromOnce e) v s
                      | (e,v,s) <- searchType db query]
