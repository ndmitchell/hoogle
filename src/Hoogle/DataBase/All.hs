
module Hoogle.DataBase.All
    (DataBase, showDataBase
    ,TextScore, TypeScore
    ,module Hoogle.DataBase.All
    ,module Hoogle.DataBase.Serialise
    ) where

import Data.Binary.Defer.Index
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.Type
import Hoogle.Item.All
import Hoogle.DataBase.Serialise
import General.Code


createDataBase :: [DataBase] -> TextBase -> DataBase
createDataBase deps xs = DataBase items
        (createNameSearch ys) (createTypeSearch as is ys)
        (createSuggest (map suggest deps) zs) as is
    where
        items = createItems xs
        ys = entriesItems items
        zs = map fst xs
        as = createAliases (map aliases deps) zs
        is = createInstances (map instances deps) zs


combineDataBase :: [DataBase] -> DataBase
combineDataBase dbs = DataBase items_
        (createNameSearch ys) (createTypeSearch as is ys)
        ss as is
    where
        items_ = mergeItems $ map items dbs
        ys = entriesItems items_
        ss = mergeSuggest $ map suggest dbs
        as = mergeAliases $ map aliases dbs
        is = mergeInstances $ map instances dbs


searchName :: DataBase -> String -> [(Link Entry,EntryView,TextScore)]
searchName db = searchNameSearch (nameSearch db)


searchType :: DataBase -> TypeSig -> [(Link Entry,[EntryView],TypeScore)]
-- although aliases and instances are given, they are usually not used
searchType db = searchTypeSearch (aliases db) (instances db) (typeSearch db)


suggestion :: [DataBase] -> TypeSig -> Maybe (Either String TypeSig)
suggestion db = askSuggest (map suggest db)
