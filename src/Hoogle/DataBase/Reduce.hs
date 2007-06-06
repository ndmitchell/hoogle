
module Hoogle.DataBase.Reduce(
    reduce
    ) where

import Hoogle.TypeSig.All
import Hoogle.Item.All
import Hoogle.Result.All

import Hoogle.DataBase.Instances
import Hoogle.DataBase.Alias

import Data.Generics.Uniplate
import Data.Maybe
import Data.List
import Control.Monad



reduce :: Instances -> Alias -> TypeSig -> TypeSig -> Maybe [TypeDiff]
reduce i a x y = reducePair i a (x,y)


type Reduce = TypeSig -> TypeSig -> Maybe ([TypeDiff], [(TypeSig,TypeSig)])


reduceList :: Instances -> Alias -> [(TypeSig,TypeSig)] -> Maybe [TypeDiff]
reduceList inst alia [] = Just []
reduceList inst alia (x:xs) = do
    a <- reducePair inst alia x
    b <- reduceList inst alia xs
    return (a++b)

reducePair :: Instances -> Alias -> (TypeSig,TypeSig) -> Maybe [TypeDiff]
reducePair inst alia (sig1,sig2) = do
    (a,ss) <- reduceOne inst alia sig1 sig2
    b <- reduceList inst alia ss
    return (a++b)

reduceOne :: Instances -> Alias -> Reduce
reduceOne inst alia s1 s2 = f (reducers inst alia)
    where
        f [] = Nothing
        f (act:xs) = act s1 s2 `mplus` f xs


reducers :: Instances -> Alias -> [Reduce]
reducers inst alia = [reduceEqual, reduceAlias alia, reduceAlpha, reduceDecompose inst alia, reduceDeadContext]


reduceEqual :: Reduce
reduceEqual t1 t2 | t1 == t2 = Just ([], [])
                  | otherwise = Nothing

reduceAlias :: Alias -> Reduce
reduceAlias alia t1 t2
    | isJust m1 && m1 >= m2 = Just (diff a1, [(followAlias alia t1, t2)])
    | isJust m2             = Just (diff a2, [(t1, followAlias alia t2)])
    | otherwise = Nothing
    where
        a1 = pick t1; a2 = pick t2
        m1 = test a1; m2 = test a2
        
        pick (TypeSig _ (TApp (TLit x) _)) = Just x
        pick (TypeSig _ (TLit x)) = Just x
        pick _ = Nothing
        
        test t = isAlias alia =<< t

        diff (Just x) = [TypeAlias x]


reduceAlpha :: Reduce
reduceAlpha t1@(TypeSig _ (TVar a1)) t2@(TypeSig _ (TVar a2)) | a1 /= a2 =
    Just ([TypeAlpha a1 a2], [(t1, renameVars (\x -> if x == a2 then a1 else x) t2)])
reduceAlpha _ _ = Nothing


reduceDecompose :: Instances -> Alias -> Reduce
reduceDecompose inst alia (TypeSig c1 (TApp t1 ts1)) (TypeSig c2 (TApp t2 ts2))
    | length ts1 == length ts2 = Just ([], zip (map (TypeSig c1) $ t1:ts1) (map (TypeSig c2) $ t2:ts2))
reduceDecompose _ _ _ _ = Nothing


reduceDeadContext :: Reduce
reduceDeadContext t1 t2 | isJust m1 || isJust m2 = Just ([], [(fromMaybe t1 m1, fromMaybe t2 m2)])
    where
        m1 = f t1; m2 = f t2

        f (TypeSig cs t) | null kill = Nothing
                         | otherwise = Just (TypeSig keep t)
            where
                free = [x | TVar x <- universe t]
                (keep,kill) = partition g cs
                g v = any (`elem` free) [x | TVar x <- universe v]
reduceDeadContext _ _ = Nothing
