
module Hoogle.DataBase.Types(
    Types, createTypes,
    searchTypes
    ) where

import Hoogle.TypeSig.All
import Hoogle.Item.All
import Hoogle.Result.All

import Hoogle.DataBase.Instances
import Hoogle.DataBase.Alias

import Data.Binary.Defer


data Types = Types [TypeInfo]
             deriving Show

data TypeInfo = TypeInfo Constraint [Type] [Permute]
                deriving Show

data Permute = Permute ItemId [Int]
               deriving Show


instance BinaryDefer Types where
    bothDefer = defer [\ ~(Types a) -> unit Types << a]

instance BinaryDefer TypeInfo where
    bothDefer = defer [\ ~(TypeInfo a b c) -> unit TypeInfo << a << b << c]

instance BinaryDefer Permute where
    bothDefer = defer [\ ~(Permute a b) -> unit Permute << a << b]


-- can be much more clever:
-- should do type signature compression
-- should rename everything so all free vars are numbers (to make alpha work)

createTypes :: [Item] -> Types
createTypes xs = Types $ concatMap f xs
    where
        f (Item{itemId=i, itemRest=ItemFunc (TypeTree (TypeSig c t))}) =
                [TypeInfo c vals [Permute i [0..length vals-2]]]
            where
                vals = splitFun t

        f x = []



searchTypes :: (Types, Instances, Alias) -> TypeSig -> [(ItemId, TypeMatch)]
searchTypes _ _ = []



