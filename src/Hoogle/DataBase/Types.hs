
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
match (inst,alia) c1 c2 t1 t2 = reduceAll inst alia (TypeSig c1 t1) (TypeSig c2 t2)


type Reduce = TypeSig -> TypeSig -> Maybe (TypeDiff, TypeSig, TypeSig)


reduceAll :: Instances -> Alias -> TypeSig -> TypeSig -> Maybe [TypeDiff]
reduceAll inst alia sig1 sig2
    | sig1 == sig2 = Just []
    | otherwise = do
        (a,s1,s2) <- reduce inst alia sig1 sig2
        b <- reduceAll inst alia s1 s2
        return (a:b)

reduce :: Instances -> Alias -> TypeSig -> TypeSig -> Maybe (TypeDiff, TypeSig, TypeSig)
reduce inst alia s1 s2 = f (reducers inst alia)
    where
        f [] = Nothing
        f (act:xs) = act s1 s2 `mplus` f xs


reducers :: Instances -> Alias -> [Reduce]
reducers inst alia = [reduceAlias alia]


reduceAlias :: Alias -> Reduce
reduceAlias alia t1 t2
    | isJust m1 && m1 >= m2 = Just (diff a1, followAlias alia t1, t2)
    | isJust m2             = Just (diff a2, t1, followAlias alia t2)
    | otherwise = Nothing
    where
        a1 = pick t1; a2 = pick t2
        m1 = test a1; m2 = test a2
        
        pick (TypeSig _ (TApp (TLit x) _)) = Just x
        pick (TypeSig _ (TLit x)) = Just x
        pick _ = Nothing
        
        test t = isAlias alia =<< t

        diff (Just x) = TypeAlias x
