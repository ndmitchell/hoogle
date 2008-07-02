
module Hoogle.DataBase.TypeSearch.All(
    createTypeSearch, TypeSearch,
    searchTypeSearch, TypeScore
    ) where

import Hoogle.DataBase.TypeSearch.Graphs
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Instances
import Hoogle.DataBase.TypeSearch.Aliases
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.Item


newtype TypeSearch = TypeSearch Graphs

instance Show TypeSearch where
    show (TypeSearch x) = show x

instance BinaryDefer TypeSearch where
    put (TypeSearch x) = put x
    get = get1 TypeSearch


---------------------------------------------------------------------
-- CREATION

createTypeSearch :: [(TextItem, Maybe Entry)] -> TypeSearch
createTypeSearch xs = TypeSearch $ graphs (instances tis) (aliases tis) types
    where
        tis = map fst xs
        types = [(newLookup (entryId e), sig) | (ItemFunc _ sig, Just e) <- xs]


---------------------------------------------------------------------
-- SEARCHING

searchTypeSearch :: TypeSearch -> Index Entry -> TypeSig -> [(Entry,EntryView,TypeScore)]
searchTypeSearch (TypeSearch g) i t =
    [(lookupIndex a i, b, c) | (a,b,c) <- graphsSearch g t]
