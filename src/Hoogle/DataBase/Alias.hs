
module Hoogle.DataBase.Alias(
    Alias, createAlias,
    isAlias, followAlias
    ) where

import Hoogle.TypeSig.All
import Hoogle.Item.All
import Hoogle.DataBase.BinaryDefer

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import Data.Binary.Defer


data Alias = Alias (Map.Map String (Int,TypeSig))
             deriving Show

instance BinaryDefer Alias where
    bothDefer = defer [\ ~(Alias a) -> unit Alias << a]


createAlias :: [Item] -> Alias
createAlias xs = Alias $ Map.fromList [(s,(i,t)) | (i,(s,t)) <- zip [0..] res]
    where res = orderAliases $ concatMap pickAlias xs


orderAliases :: [(String,TypeSig)] -> [(String,TypeSig)]
orderAliases xs = f $ reqAlias (map fst xs) xs
    where
        f [] = []
        f (x:xs) = map fst yes ++ f (map (g del) no)
            where
                (yes,no) = partition (null . snd) (x:xs)
                (yes2,no2) = if null yes then ([x],xs) else (yes,no)
                del = map (fst . fst) yes

        g del (a,b) = (a, filter (`notElem` del) b)


reqAlias :: [String] -> [(String,TypeSig)] -> [((String,TypeSig),[String])]
reqAlias list xs = [((a,b), f b) | (a,b) <- xs]
    where
        f (TypeSig a b) = concatMap g (b:a)
    
        g (TApp x xs) = concatMap g (x:xs)
        g (TFun xs) = concatMap g xs
        g (TLit x) = [x | x `elem` list]
        g _ = []


pickAlias :: Item -> [(String,TypeSig)]
pickAlias Item{itemName=name, itemRest=ItemAlias (LhsTree con1 free) (TypeTree (TypeSig con2 typ))} =
        [(name, TypeSig (map f $ con1 ++ con2) (f typ))]
    where
        ren = zip free $ map (:[]) ['a'..]
        
        f (TApp x xs) = TApp (f x) (map f xs)
        f (TFun xs) = TFun (map f xs)
        f (TVar x) = TVar $ fromMaybe x $ lookup x ren
        f x = x

pickAlias _ = []



isAlias :: Alias -> String -> Maybe Int
isAlias (Alias a) x = liftM fst $ Map.lookup x a


followAlias :: Alias -> TypeSig -> TypeSig
followAlias (Alias a) (TypeSig con1 (TApp (TLit name) vars)) =
        TypeSig (con1 ++ map f con2) (f typ)
    where
        (TypeSig con2 typ) = snd $ fromJust $ Map.lookup name a

        f (TApp x xs) = TApp (f x) (map f xs)
        f (TFun xs) = TFun (map f xs)
        f (TVar [x]) | i >= 0 && i < length vars = vars !! i
            where i = ord x - ord 'a'
        f x = x
