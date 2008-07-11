
module Hoogle.DataBase.Instances(
    Instances, createInstances, normContext, followInstances
    ) where

import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Data.Binary.Defer
import qualified Data.Map as Map
import Data.Generics.Uniplate


newtype Instances = Instances (Map.Map String [String])

instance Show Instances where
    show (Instances mp) = unlines $ map f $ Map.toList mp
        where f (v,cs) = "instance " ++ v ++ " <= " ++ unwords cs


instance BinaryDefer Instances where
    put (Instances x) = put1 x
    get = get1 Instances


createInstances :: [TextItem] -> Instances
createInstances xs = Instances $ foldl f Map.empty ys
    where
        ys = [(v, c) | ItemInstance (TypeSig [] (TApp (TLit c) vs)) <- xs, TLit v <- vs]
        f mp (v,c) = Map.insertWith (++) v [c] mp


-- PostCondition: All classes must be "TApp (TLit x) [TVar y]"
-- Convert:
--    MPTC a b |-> MPTC1 a, MPTC2 b
--    C (M a) |-> C a
-- Do not load Instances ever
normContext :: Instances -> TypeSig -> TypeSig
normContext _ (TypeSig a b) = TypeSig con b
    where con = [TApp (TLit c) [TVar v] | TApp (TLit c) xs <- a, x <- xs, TVar v <- universe x]


followInstances :: Instances -> TypeSig -> [TypeSig]
followInstances _ _ = []
