module Hoogle.DataBase.All
    (DataBase, showDataBase
    ,module Hoogle.DataBase.All
    ,module Hoogle.DataBase.Serialise
    ) where

import Hoogle.Store.All
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
        ns = createSubstrSearch [(k, y) | y <- ys, let k = entryKey $ fromOnce y, k /= ""]
        as = createAliases (map aliases deps) facts
        is = createInstances (map instances deps) facts
        tys = [(sig, x) | x <- ys, Just sig <- [entryType $ fromOnce x]]


createDataBaseEntries :: Input -> DataBase
createDataBaseEntries (facts,xs) = DataBase (createItems xs) (createSubstrSearch []) (createTypeSearch mempty mempty []) mempty mempty mempty

createDataBaseText :: [Once Entry] -> DataBase
createDataBaseText ys = DataBase mempty ns (createTypeSearch mempty mempty []) mempty mempty mempty
    where ns = createSubstrSearch [(k, y) | y <- ys, let k = entryKey $ fromOnce y, k /= ""]

createDataBaseType :: [DataBase] -> Input -> [Once Entry] -> DataBase
createDataBaseType deps (facts,_) ys = DataBase mempty
        (createSubstrSearch []) (createTypeSearch as is tys)
        (createSuggest (map suggest deps) facts) as is
    where
        as = createAliases (map aliases deps) facts
        is = createInstances (map instances deps) facts
        tys = [(sig, x) | x <- ys, Just sig <- [entryType $ fromOnce x]]


combineDataBase :: [DataBase] -> DataBase
combineDataBase [db] = db
combineDataBase dbs = DataBase items_
        ns (createTypeSearch as is tys)
        ss as is
    where
        items_ = mconcat $ map items dbs
        ys = entriesItems items_
        ns = createSubstrSearch [(entryKey $ fromOnce y, y) | y <- ys]
        ss = mconcat $ map suggest dbs
        as = mconcat $ map aliases dbs
        is = mconcat $ map instances dbs
        tys = [(sig, x) | x <- ys, Just sig <- [entryType $ fromOnce x]]


searchName :: DataBase -> String -> [(Once Entry,EntryView,Score)]
searchName db = searchSubstrSearch (nameSearch db)

searchExactName :: ItemKind -> DataBase -> String -> [(Once Entry,EntryView,Score)]
searchExactName kind db = filter' . searchExactSearch (nameSearch db)
  where filter' = if kind == UnclassifiedItem
                  then id
                  else filter (\(ent,_,_) -> kind == entryKind (fromOnce ent))


searchType :: DataBase -> TypeSig -> [(Once Entry,[EntryView],Score)]
-- although aliases and instances are given, they are usually not used
searchType db = searchTypeSearch (aliases db) (instances db) (typeSearch db)


suggestion :: [DataBase] -> TypeSig -> Maybe (Either String TypeSig)
suggestion db = askSuggest (map suggest db)


completions :: DataBase -> String -> [String]
completions db = completionsSubstrSearch (nameSearch db)
