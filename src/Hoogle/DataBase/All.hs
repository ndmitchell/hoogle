
module Hoogle.DataBase.All
    (DataBase
    ,TextScore
    ,module Hoogle.DataBase.All
    ,module Hoogle.DataBase.Item
    ,module Hoogle.DataBase.Serialise
    ) where

import Data.Binary.Defer.Index
import Hoogle.TextBase.All
import Hoogle.DataBase.Type
import Hoogle.DataBase.Item
import Hoogle.DataBase.Serialise


createDataBase :: [TextItem] -> DataBase
createDataBase xs = DataBase items
        (createTextSearch ys) (createTypeSearch ys) (createSuggest ys)
    where (items,ys) = createItems xs


searchText :: DataBase -> String -> [(Entry,EntryView,TextScore)]
searchText db = searchTextSearch (textSearch db) (entries $ items db)


entryParents :: DataBase -> Entry -> Maybe (Module, Package)
entryParents db e = case entryModule e of
    Nothing -> Nothing
    Just i -> let m = lookupIndex i (modules $ items db)
                  p = lookupIndex (modulePackage m) (packages $ items db)
              in Just (m,p)
