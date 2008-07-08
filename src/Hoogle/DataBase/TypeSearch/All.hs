
-- TODO: Aliases and Instances from imported packages should be
-- used when searching.

module Hoogle.DataBase.TypeSearch.All(
    createTypeSearch, TypeSearch,
    searchTypeSearch, TypeScore
    ) where

import Hoogle.DataBase.TypeSearch.Graphs
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
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

createTypeSearch :: Aliases -> Instances -> [(TextItem, Maybe (Link Entry))] -> TypeSearch
createTypeSearch aliases instances xs = TypeSearch $ newGraphs aliases instances types
    where types = [(e, sig) | (ItemFunc _ sig, Just e) <- xs]


---------------------------------------------------------------------
-- SEARCHING

searchTypeSearch :: Aliases -> Instances -> TypeSearch -> TypeSig -> [(Link Entry,[EntryView],TypeScore)]
searchTypeSearch as is (TypeSearch g) t =
    [(a, b, c) | (a,b,c) <- graphsSearch as is g t]
