
module Hoogle.DataBase.Types(saveTypes, searchTypes) where

import System.IO
import Data.List
import Control.Exception

import General.All
import Hoogle.Common.All
import Hoogle.TypeSig.All

{-
FORMAT

n = number of distinct types
then n*type-item

type-item =
    arity = int
    type structure of each of the components

    count = int
    count*{id,int*arity} -- the permutations

If two types are equal up to alpha renaming and parameter reordering
then they are merged into one type value.
-}


data TypeItem = TypeItem Constraint [TypeSig] TypeSig [Permute]
data Permute = Permute Int [Int]



hPutTypes :: Handle -> [Type] -> IO ()
hPutTypes hndl xs = do
    hPutInt hndl (length xs)
    mapM_ (hPutType hndl) xs


hPutType :: Handle -> Type -> IO ()
hPutType hndl x =
    case x of
        TApp x xs -> hPutByte hndl 0 >> hPutTypes hndl (x:xs)
        TLit x ->    hPutByte hndl 1 >> hPutString hndl x
        TVar x ->    hPutByte hndl 2 >> hPutString hndl x
        TFun xs ->   hPutByte hndl 4 >> hPutTypes hndl xs


saveTypes :: Handle -> [Item] -> IO [Response]
saveTypes hndl items =
    do
        hPutInt hndl (length typeList)
        mapM_ output typeList
        return []
    where
        output (sig@(TypeSig con typ), perms) = do
                hPutInt hndl arity
                hPutTypes hndl con
                mapM_ (hPutType hndl) typs
                hPutInt hndl (length perms)
                mapM_ f perms
            where
                typs = splitFun typ
                arity = length typs - 1
                f (Permute n xs) = assert (length xs == arity) $ mapM_ (hPutInt hndl) (n:xs)
            
            
    
        typeList :: [(TypeSig,[Permute])]
        typeList = map g $ groupBy eqFst [(t,f t idn) | Item{itemType=Just (TypeAST t), itemId=Just idn} <- items]
            where
                g xs = (fst (head xs), map snd (tail xs))
            
                f (TypeSig _ a) idn = Permute idn [0.. length n - 2]
                    where n = splitFun a
        
        eqFst (a,_) (b,_) = a == b



searchTypes :: Handle -> TypeSig -> IO [Result]
searchTypes _ _ = return []
