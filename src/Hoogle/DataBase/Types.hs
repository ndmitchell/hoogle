
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
import Data.Maybe


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
searchTypes (Types dbt,dbi,dba) (TypeSig c1 t) = concatMap f dbt
    where
        t1 = splitFun t
        f (TypeInfo c2 t2 perm) = [(i, TypeMatch [] d) | (m,d) <- ans, Permute i _ <- perm]
            where ans = matchPermute (dbi,dba) c1 c2 t1 t2



type Mapping = [(Int,Int)]

matchPermute :: (Instances,Alias) -> Constraint -> Constraint -> [Type] -> [Type] -> [(Mapping,[TypeDiff])]
matchPermute d c1 c2 t1 t2 | length t1 == length t2 = 
    case matches d c1 c2 t1 t2 of
        Nothing -> []
        Just y -> [(zip [0..] [0..length t1 - 2], y)]
matchPermute _ _ _ _ _ = []


matches d c1 c2 ts1 ts2 = sequence (zipWith (match d c1 c2) ts1 ts2) >>= return . concat


match :: (Instances,Alias) -> Constraint -> Constraint -> Type -> Type -> Maybe [TypeDiff]
match d@(inst,alia) c1 c2 t1 t2 = f (norm t1) (norm t2)
    where
        f a1@(TApp (TLit t1) u1) a2@(TApp (TLit t2) u2)
            | t1 == t2 = matches d c1 c2 u1 u2
            | isJust m1 && m1 >= m2 = let (c_1,a_1) = follow c1 a1 in cont c_1 c2 a_1 a2
            | isJust m2 =             let (c_2,a_2) = follow c2 a2 in cont c1 c_2 a1 a_2
            where
                m1 = isAlias alia t1
                m2 = isAlias alia t2

                follow c1 a1 = let TypeSig c_1 a_1 = followAlias alia (TypeSig c1 a1) in (c_1, a_1)
                cont c1 c2 a1 a2 = match d c1 c2 a1 a2 >>= return . (TypeAlias:)

        f _ _ = Nothing


        norm (TLit x) = TApp (TLit x) []
        norm x = x
