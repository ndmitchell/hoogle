
module Hoogle.Search.All where

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Common.All



-- return all the results
searchAll :: DataBase -> Query -> IO [Result]
searchAll database query = getResults database query


-- should be possible to fast-path certain searches, currently not done
searchRange :: DataBase -> Query -> Int -> Int -> IO [Result]
searchRange database query from to = do
    res <- getResults database query
    return $ take to $ drop from res


getResults :: DataBase -> Query -> IO [Result]
getResults database query = return []
