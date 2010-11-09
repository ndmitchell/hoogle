
module Hoogle.Search.All(
    Result(..), renderResult,
    searchAll, searchRange
    ) where

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Result
import Hoogle.Search.Results
import Hoogle.TypeSig.All


-- return all the results
searchAll :: [DataBase] -> Query -> [Result]
searchAll databases query = getResults query databases


-- should be possible to fast-path certain searches, currently not done
-- start index, end index
searchRange :: (Int,Int) -> [DataBase] -> Query -> [Result]
searchRange (from,to) databases query = take (to - from + 1) $ drop from $ getResults query databases


getResults :: Query -> [DataBase] -> [Result]
getResults query = mergeDataBaseResults . map (mergeQueryResults query . f)
    where
        f d = [typeSearch d q | Just q <- [typeSig query]] ++
              map (nameSearch d) (names query)


nameSearch :: DataBase -> String -> [Result]
nameSearch db query = [Result e [v] s
                      | (e,v,s) <- searchName db query]


typeSearch :: DataBase -> TypeSig -> [Result]
typeSearch db query = [Result e v s
                      | (e,v,s) <- searchType db query]
