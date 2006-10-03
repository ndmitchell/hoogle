
module Hoogle.DataBase.Types(saveTypes, searchTypes) where

import System.IO
import Data.List
import Control.Exception
import Control.Monad

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


data TypeItem = TypeItem Constraint [Type] Type [Permute]
                deriving Show

data Permute = Permute Int [Int]
               deriving Show



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
        TFun xs ->   hPutByte hndl 3 >> hPutTypes hndl xs


hGetTypes :: Handle -> IO [Type]
hGetTypes hndl = do
    n <- hGetInt hndl
    replicateM n (hGetType hndl)


hGetType :: Handle -> IO Type
hGetType hndl = do
    i <- hGetByte hndl
    case i of
        0 -> hGetTypes hndl >>= \(x:xs) -> return $ TApp x xs
        1 -> liftM TLit $ hGetString hndl
        2 -> liftM TVar $ hGetString hndl
        3 -> liftM TFun $ hGetTypes hndl


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



searchTypes :: Handle -> TypeSig -> IO [[Result]]
searchTypes hndl (TypeSig tcon ttyp) = do
        count <- hGetInt hndl
        liftM (filter (not . null)) $ replicateM count (liftM match $ readTypeItem)
    where
        readTypeItem = do
            arity <- hGetInt hndl
            con <- hGetTypes hndl
            typs <- replicateM arity (hGetType hndl)
            res <- hGetType hndl
            nperms <- hGetInt hndl
            perms <- replicateM nperms (readPerm arity)
            return $ TypeItem con typs res perms
        
        readPerm arity = do
            idn <- hGetInt hndl
            xs <- replicateM arity (hGetInt hndl)
            return $ Permute idn xs
            
        titems = splitFun ttyp
        (targs,tres) = (init titems, last titems)

        match :: TypeItem -> [Result]
        match (TypeItem con args res perms) =
            case matchTypes (tres:targs) (res:args) of
                Nothing -> []
                Just (bad,vars) -> map (f $ bad++varsToBad vars) perms
            where
                f diff (Permute idn order) = Result Nothing (Just $ TypeMatch diff order) blankItem{itemId=Just idn}


        varsToBad :: [(String,String)] -> [TypeDiff]
        varsToBad xs = replicate (length (a \\ nub a)) MultiLeft ++ replicate (length (b \\ nub b)) MultiRight
            where (a,b) = unzip $ nub xs


-- do not worry about type aliases for now
matchType :: Type -> Type -> Maybe ([TypeDiff], [(String,String)])
matchType (TFun xs) (TFun ys) = matchTypes xs ys
matchType (TLit x) (TLit y) = if x == y then Just ([],[]) else Nothing
matchType (TVar x) (TVar y) = Just ([],[(x,y)])
matchType (TApp x xs) (TApp y ys) = matchTypes (x:xs) (y:ys)

matchType x@(TVar _) y = addErr UnwrapRight $ matchTypesEq (repeat x) (fromList y)
matchType x y@(TVar _) = addErr UnwrapLeft  $ matchTypesEq (fromList x) (repeat y)

matchType _ _ = Nothing


addErr msg Nothing = Nothing
addErr msg (Just (x,y)) = Just (msg:x,y)


fromList (TLit x) = []
fromList (TFun xs) = xs
fromList (TApp x xs) = xs



matchTypes :: [Type] -> [Type] -> Maybe ([TypeDiff], [(String,String)])
matchTypes xs ys | length xs /= length ys = Nothing
                 | otherwise = matchTypesEq xs ys


matchTypesEq :: [Type] -> [Type] -> Maybe ([TypeDiff], [(String,String)])
matchTypesEq xs ys = liftM f $ sequence $ zipWith matchType xs ys
    where f x = let (a,b) = unzip x in (concat a, concat b)
