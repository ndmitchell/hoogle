
module Hoogle.DataBase.All
    (DataBase
    ,module Hoogle.DataBase.All
    ,module Hoogle.DataBase.Serialise
    ) where

import Hoogle.TextBase.All
import Hoogle.DataBase.Type
import Hoogle.DataBase.Serialise


createDataBase :: [TextItem] -> DataBase
createDataBase xs = DataBase items
        (createTextSearch ys) (createTypeSearch ys) (createSuggest ys)
    where (items,ys) = createItems xs


searchText :: DataBase -> String -> [(Entry,EntryView,TextScore)]
searchText db = searchTextSearch (textSearch db) (entries $ items db)
