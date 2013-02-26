
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
        f d = [ typeSearch d q
              | Just q <- [typeSig query], not (exactSearch query) ] ++
              map (nameSearch d (exactSearch query)) (names query)


nameSearch :: DataBase -> Bool -> String -> [Result]
nameSearch db exact query =
    [ Result (fromOnce e) [v] s
    | (e,v,s) <- (if exact then searchExactName else searchName) db query ]


typeSearch :: DataBase -> TypeSig -> [Result]
typeSearch db query = [Result (fromOnce e) v s
                      | (e,v,s) <- searchType db query]
