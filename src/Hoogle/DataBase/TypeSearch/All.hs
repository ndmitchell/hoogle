
module Hoogle.DataBase.TypeSearch.All where

import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.DataBase.Item
import Hoogle.TypeSig.All


data TypeSearch = TypeSearch deriving Show

instance BinaryDefer TypeSearch where
    put _ = return ()
    get = return TypeSearch


---------------------------------------------------------------------
-- CREATION

createTypeSearch :: a -> TypeSearch
createTypeSearch _ = TypeSearch


---------------------------------------------------------------------
-- SEARCHING

data TypeScore = TypeScore
                 deriving (Eq,Ord)

instance Show TypeScore where
    show TypeScore = "_"


searchTypeSearch :: TypeSearch -> Index Entry -> TypeSig -> [(Entry,EntryView,TypeScore)]
searchTypeSearch _ _ _ = []
