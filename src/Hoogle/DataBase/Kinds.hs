
module Hoogle.DataBase.Kinds(Kinds, saveKinds, loadKinds, checkTypeKind, checkClassKind) where

import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Data.List
import qualified Data.Map as Map
import System.IO


-- roughly kinds, i.e. [] 1, () 0, Maybe 1
-- some types may have multiple kinds, if the scoping comes into play
-- i.e. Data.Map1.Map 1, Data.Map2.Map 2 - for example
-- but would be a really bad idea!
type KindMap = Map.Map String [Int]

data Kinds = Kinds {kindsClass :: KindMap, kindsType :: KindMap}


saveKinds :: Handle -> TextBase -> IO [String]
saveKinds hndl tb = return errs
    where
        errs = getErrs "Class" (kindsClass res) ++ getErrs "Type" (kindsType res)
            where
                getErrs msg x = concatMap (getErr msg) (Map.toList x)
            
                getErr msg (a,[x]) = []
                getErr msg (a,xs) = ["Warning: " ++ msg ++ " has multiple kinds, " ++ a ++ " has " ++ show xs]

    
        res = foldr f (Kinds Map.empty Map.empty) tb
        
        f x k = case x of
            Class x -> fClass x k
            Func _ x -> fType x k
            TypeAlias x y -> fType x $ fType y k
            Data _ x -> fType x k
            Instance x -> fClass x k
            _ -> k

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
       
        


loadKinds :: Handle -> IO Kinds
loadKinds hndl = error "todo"


checkTypeKind :: Kinds -> String -> Int -> Bool
checkTypeKind kinds name num = checkKind (kindsType kinds) name num

checkClassKind :: Kinds -> String -> Int -> Bool
checkClassKind kinds name num = checkKind (kindsClass kinds) name num


checkKind :: KindMap -> String -> Int -> Bool
checkKind kmap name num = num `elem` Map.findWithDefault [] name kmap
