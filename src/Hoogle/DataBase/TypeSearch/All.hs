
-- TODO: Aliases and Instances from imported packages should be
-- used when searching.

module Hoogle.DataBase.TypeSearch.All(
    createTypeSearch, TypeSearch,
    searchTypeSearch, TypeScore
    ) where

import Hoogle.DataBase.TypeSearch.Graphs
import Hoogle.DataBase.TypeSearch.TypeScore
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.Store.All
import Hoogle.Type.All
import Hoogle.Score.All
import General.Base


newtype TypeSearch = TypeSearch Graphs

instance NFData TypeSearch where
    rnf (TypeSearch a) = rnf a

instance Show TypeSearch where
    show (TypeSearch x) = show x

instance Store TypeSearch where
    put (TypeSearch x) = put x
    get = get1 TypeSearch


---------------------------------------------------------------------
-- CREATION

createTypeSearch :: Aliases -> Instances -> [(TypeSig, Once Entry)] -> TypeSearch
createTypeSearch aliases instances xs = TypeSearch $ newGraphs aliases instances xs


---------------------------------------------------------------------
-- SEARCHING

searchTypeSearch :: Aliases -> Instances -> TypeSearch -> TypeSig -> [(Once Entry,[EntryView],Score)]
searchTypeSearch as is (TypeSearch g) t =
    [(a, b, typeScore $ costsTypeScore c) | (a,b,c) <- graphsSearch as is g t]
