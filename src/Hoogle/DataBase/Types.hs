
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
import Data.Generics.Uniplate
import Data.Maybe
import Data.List
import Control.Monad


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


type Reduce = TypeSig -> TypeSig -> Maybe [TypeDiff]

searchTypes :: Types -> Reduce -> TypeSig -> [(ItemId, TypeMatch)]
searchTypes (Types dbt) reduce orig = concatMap f dbt
    where
        TypeSig c1 t = renameVars ('$':) orig
        t1 = splitFun t
        f (TypeInfo c2 t2 perm) = [(i, TypeMatch [] d) | (m,d) <- ans, Permute i _ <- perm]
            where ans = matchPermute reduce c1 c2 t1 t2



type Mapping = [(Int,Int)]

matchPermute :: Reduce -> Constraint -> Constraint -> [Type] -> [Type] -> [(Mapping,[TypeDiff])]
matchPermute d c1 c2 t1 t2 | length t1 == length t2 = 
    case matches d c1 c2 t1 t2 of
        Nothing -> []
        Just y -> [(zip [0..] [0..length t1 - 2], y)]
matchPermute _ _ _ _ _ = []


matches d c1 c2 ts1 ts2 = sequence (zipWith (match d c1 c2) ts1 ts2) >>= return . concat


match :: Reduce -> Constraint -> Constraint -> Type -> Type -> Maybe [TypeDiff]
match reduce c1 c2 t1 t2 = reduce (TypeSig c1 t1) (TypeSig c2 t2)
