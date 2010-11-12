
module Hoogle.DataBase.Instances(
    Instances, createInstances, mergeInstances,
    normInstances, hasInstance
    ) where

import General.Code
import Hoogle.Type.All
import Hoogle.TypeSig.All
import Data.Binary.Defer
import qualified Data.Binary.Defer.Map as Map


-- Map type [classes]
newtype Instances = Instances {fromInstances :: Map.Map String [String]}

instance Show Instances where
    show (Instances mp) = unlines $ map f $ Map.toList mp
        where f (v,cs) = "instance " ++ v ++ " <= " ++ unwords cs


instance BinaryDefer Instances where
    put = put1 . fromInstances
    get = get1 Instances


createInstances :: [Instances] -> [Fact] -> Instances
createInstances deps xs = mergeInstances (i:deps)
    where
        i = Instances $ foldl f Map.empty ys
        ys = [(v, c) | FactInstance (TypeSig [] (TApp (TLit c) vs)) <- xs, TLit v <- vs]
        f mp (v,c) = Map.insertWith (++) v [c] mp


mergeInstances :: [Instances] -> Instances
mergeInstances = Instances . Map.unionsWith (\x y -> nub $ x ++ y) . map fromInstances




-- Convert:
--    MPTC a b |-> MPTC a, MPTC b
--    C (M a) |-> C a
-- Do not load Instances ever
normInstances :: Instances -> TypeSig -> TypeSimp
normInstances _ (TypeSig a b) = TypeSimp con b
    where
        con = sort $ nub [(c,v) | TApp (TLit c) xs <- a, x <- xs, v <- variables x, v `elem` vs]
        vs = variables b


-- hasInstance _ C M, does C M exist
hasInstance :: Instances -> String -> String -> Bool
hasInstance (Instances mp) c m = c `elem` Map.findWithDefault [] m mp
