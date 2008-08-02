
module Hoogle.DataBase.Aliases(
    Aliases, createAliases, normAliases
    ) where

import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import qualified Data.Binary.Defer.Map as Map
import Data.Binary.Defer
import Data.Generics.Uniplate
import General.Code


newtype Aliases = Aliases (Map.Map String Alias)

instance BinaryDefer Aliases where
    put (Aliases a) = put a
    get = get1 Aliases

instance Show Aliases where
    show (Aliases mp) = unlines [ unwords $ "type" : s : vs ++ ["=", show t]
                                | (s,Alias vs t) <- Map.toList mp]


data Alias = Alias
    {args :: [String] -- the free variables
    ,rhs :: Type -- the resulting type
    }

instance BinaryDefer Alias where
    put (Alias a b) = put2 a b
    get = get2 Alias


createAliases :: [TextItem] -> Aliases
createAliases ti = Aliases $ transitiveClosure $ Map.fromList
    [ (name, Alias [v | TVar v <- args] rhs)
    | ItemAlias (TypeSig _ lhs) (TypeSig _ rhs) <- ti
    , let (TLit name, args) = fromTApp lhs]


-- Must be careful with aliases which expand back to themselves
-- i.e. template-haskell has "type Doc = PprM Doc"
-- probably the result of unqualifying names
transitiveClosure :: Map.Map String Alias -> Map.Map String Alias
transitiveClosure mp = Map.mapWithKey (\k x -> x{rhs = f [k] $ rhs x}) mp
    where
        f :: [String] -> Type -> Type
        f seen t = case [(name,x) | (name,x) <- followAliases (Aliases mp) t, name `notElem` seen] of
                        [] -> t
                        (name,x):_ -> f (name:seen) x


-- perform a 1-step alias following
followAliases :: Aliases -> Type -> [(String,Type)]
followAliases as t =
    [ (s, gen x2)
    | (x, gen) <- contexts t
    , Just (s,x2) <- [followAlias as x]]


followAlias :: Aliases -> Type -> Maybe (String, Type)
followAlias (Aliases mp) (TApp (TLit x) xs)
    | isJust m && length xs == length vs = Just (x, transform f rhs)
    where m@ ~(Just (Alias vs rhs)) = Map.lookup x mp
          rep = zip vs xs
          f (TVar v) = lookupJustDef (TVar v) v rep
          f x = x
followAlias as (TLit x) = followAlias as (TApp (TLit x) [])
followAlias _ _ = Nothing


normAliases :: Aliases -> Type -> ([String], Type)
normAliases as t = (sort . nub *** id) $ f t
    where
        f t = case followAlias as t of
                  Just (s,t) -> ([s],t)
                  Nothing -> (concat *** gen) $ unzip $ map f cs
            where (cs, gen) = uniplate t
