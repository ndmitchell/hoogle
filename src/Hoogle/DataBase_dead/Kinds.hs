
module Hoogle.DataBase.Kinds(Kinds, createKinds, checkTypeKind, checkClassKind, getNameKinds) where

import Hoogle.TypeSig.All
import Hoogle.Item.All

import qualified Data.Map as Map
import Data.List
import Data.Binary.Defer


-- roughly kinds, i.e. [] 1, () 0, Maybe 1
-- some types may have multiple kinds, if the scoping comes into play
-- i.e. Data.Map1.Map 1, Data.Map2.Map 2 - for example
-- but would be a really bad idea!
type KindMap = Map.Map String [Int]

data Kinds = Kinds {kindsClass :: KindMap, kindsType :: KindMap}
             deriving Show

instance BinaryDefer Kinds where
    bothDefer = defer [\ ~(Kinds a b) -> unit Kinds << a << b]


createKinds :: [Item] -> Kinds
createKinds = foldr f (Kinds Map.empty Map.empty)
    where
        f item k =
            case itemRest item of
                ItemClass lhs -> fClass (joinLhs lhs) k
                ItemFunc (TypeTree sig) -> fType sig k
                ItemAlias lhs (TypeTree rhs) -> fType (joinLhs lhs) $ fType rhs k
                ItemData _ lhs -> fType (joinLhs lhs) k
                ItemInstance x -> fClass x k
                _ -> k
            where
                joinLhs (LhsTree con free) = TypeSig con (TApp (TLit (itemName item)) (map TVar free))

        fClass (TypeSig cons x) k = foldr gClass k (x:cons)
        fType (TypeSig cons x) k = gType x $ foldr gClass k cons
        
        gClass (TApp (TLit x) xs) k = addKind k 0 x (length xs)
        
        gType (TApp (TLit x) xs) k = gTypes xs $ addKind k 1 x (length xs)
        gType (TApp x xs) k = gTypes (x:xs) k
        gType (TLit x) k = addKind k 1 x 0
        gType (TFun xs) k = gTypes xs k
        gType (TVar x) k = k
        
        gTypes xs k = foldr gType k xs
        
        
        addKind k i name num = if i == 0 then k{kindsClass=res} else k{kindsType=res}
            where
                res = Map.insertWith (\a b -> nub $ a ++ b) name [num] kp
                kp = (if i == 0 then kindsClass else kindsType) k



checkTypeKind :: Kinds -> String -> Int -> Bool
checkTypeKind kinds name num = checkKind (kindsType kinds) name num

checkClassKind :: Kinds -> String -> Int -> Bool
checkClassKind kinds name num = checkKind (kindsClass kinds) name num

checkKind :: KindMap -> String -> Int -> Bool
checkKind kmap name num = num `elem` Map.findWithDefault [] name kmap


getNameKinds :: Kinds -> String -> [Int]
getNameKinds kinds name = f (kindsType kinds) `union` f (kindsClass kinds)
    where f mp = Map.findWithDefault [] name mp

