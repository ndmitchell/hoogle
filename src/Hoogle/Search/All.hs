
module Hoogle.Search.All where

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Type



-- return all the results
searchAll :: DataBase -> Query -> IO [Result]
searchAll database query = searchRange database query 0 maxBound


searchRange :: DataBase -> Query -> Int -> Int -> IO [Result]
searchRange database query _ _ = return []

