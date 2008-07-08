
module Hoogle.DataBase.All
    (DataBase, showDataBase
    ,TextScore, TypeScore
    ,module Hoogle.DataBase.All
    ,module Hoogle.DataBase.Item
    ,module Hoogle.DataBase.Serialise
    ) where

import Data.Binary.Defer.Index
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.Type
import Hoogle.DataBase.Item
import Hoogle.DataBase.Serialise


createDataBase :: TextBase -> DataBase
createDataBase xs = DataBase items
        (createNameSearch ys) (createTypeSearch aliases instances ys)
        (createSuggest ys) aliases instances
    where
        (items,ys) = createItems xs
        zs = map fst ys
        aliases = createAliases zs
        instances = createInstances zs


searchName :: DataBase -> String -> [(Link Entry,EntryView,TextScore)]
searchName db = searchNameSearch (nameSearch db)


searchType :: DataBase -> TypeSig -> [(Link Entry,[EntryView],TypeScore)]
-- although aliases and instances are given, they are usually not used
searchType db = searchTypeSearch (aliases db) (instances db) (typeSearch db)


suggestion :: [DataBase] -> TypeSig -> Maybe (Either String TypeSig)
suggestion db = askSuggest (map suggest db)
