
module Hoogle.DataBase.All
    (DataBase, showDataBase
    ,module Hoogle.DataBase.All
    ,module Hoogle.DataBase.Serialise
    ) where

import Data.Binary.Defer.Index
import Data.Monoid
import Hoogle.DataBase.Type
import Hoogle.Type.All
import Hoogle.Score.All
import Hoogle.DataBase.Serialise


createDataBase :: [DataBase] -> Input -> DataBase
createDataBase deps (facts,xs) = DataBase items
        ns (createTypeSearch as is tys)
        (createSuggest (map suggest deps) facts) as is
    where
        items = createItems xs
        ys = entriesItems items
        ns = createSubstrSearch [(entryKey $ fromLink y, y) | y <- ys]
        as = createAliases (map aliases deps) facts
        is = createInstances (map instances deps) facts
        tys = [(sig, x) | x <- ys, Just sig <- [entryType $ fromLink x]]



combineDataBase :: [DataBase] -> DataBase
combineDataBase dbs = DataBase items_
        ns (createTypeSearch as is tys)
        ss as is
    where
        items_ = mconcat $ map items dbs
        ys = entriesItems items_
        ns = createSubstrSearch [(entryKey $ fromLink y, y) | y <- ys]
        ss = mconcat $ map suggest dbs
        as = mconcat $ map aliases dbs
        is = mconcat $ map instances dbs
        tys = [(sig, x) | x <- ys, Just sig <- [entryType $ fromLink x]]


searchName :: DataBase -> String -> [(Link Entry,EntryView,Score)]
searchName db = searchSubstrSearch (nameSearch db)


searchType :: DataBase -> TypeSig -> [(Link Entry,[EntryView],Score)]
-- although aliases and instances are given, they are usually not used
searchType db = searchTypeSearch (aliases db) (instances db) (typeSearch db)


suggestion :: [DataBase] -> TypeSig -> Maybe (Either String TypeSig)
suggestion db = askSuggest (map suggest db)


completions :: DataBase -> String -> [String]
completions db = completionsSubstrSearch (nameSearch db)
