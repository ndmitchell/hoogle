
module Hoogle.DataBase.Instances(
    Instances, createInstances, normContext, followInstances
    ) where

import General.Code
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Data.Binary.Defer
import qualified Data.Binary.Defer.Map as Map
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


-- Convert:
--    MPTC a b |-> MPTC a, MPTC b
--    C (M a) |-> C a
-- Do not load Instances ever
normContext :: Instances -> TypeSig -> TypeSimp
normContext _ (TypeSig a b) = TypeSimp con b
    where
        con = [(c,v) | TApp (TLit c) xs <- a, x <- xs, v <- variables x, v `elem` vs]
        vs = variables b


-- M |-> C _a => _a, if there is a C M instance
followInstances :: Instances -> TypeSimp -> [((String,String),TypeSimp)]
followInstances (Instances mp) (TypeSimp c t) =
        [((n,m),TypeSimp ((n,"_a"):c) (gen $ TVar "_a"))
        |(TLit m,gen) <- contexts t, n <- Map.findWithDefault [] m mp]
