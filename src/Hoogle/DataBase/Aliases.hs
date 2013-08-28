{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.DataBase.Aliases(
    Aliases, createAliases, normAliases
    ) where

import Hoogle.Type.All
import qualified Data.Map as Map
import Hoogle.Store.All
import Data.Generics.Uniplate
import General.Base
import Safe


newtype Aliases = Aliases {fromAliases :: Map.Map String Alias}

instance NFData Aliases where
    rnf (Aliases a) = rnf a

instance Store Aliases where
    put = put . fromAliases
    get = get1 Aliases

instance Show Aliases where
    show (Aliases mp) = unlines [ unwords $ "type" : s : vs ++ ["=", show t]
                                | (s,Alias vs t) <- Map.toList mp]


data Alias = Alias
    {_args :: [String] -- the free variables
    ,rhs :: Type -- the resulting type
    }
    deriving Typeable

instance NFData Alias where
    rnf (Alias a b) = rnf (a,b)

instance Store Alias where
    put (Alias a b) = put2 a b
    get = get2 Alias


createAliases :: [Aliases] -> [Fact] -> Aliases
createAliases deps ti = mergeAliases (a:deps)
    where
        a = Aliases $ transitiveClosure $ Map.fromList
            [ (name, Alias [v | TVar v <- args] rhs)
            |  FactAlias (TypeSig _ lhs) (TypeSig _ rhs) <- ti
            , let (TLit name, args) = fromTApp lhs]


-- the first is the most important
instance Monoid Aliases where
    mempty = mergeAliases []
    mappend x y = mergeAliases [x,y]
    mconcat = mergeAliases


mergeAliases :: [Aliases] -> Aliases
mergeAliases [x] = x
mergeAliases xs = Aliases $ transitiveClosure $ Map.unions $ map fromAliases xs


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
normAliases as t = first (sort . nub) $ f t
    where
        f t = case followAlias as t2 of
                   Nothing -> (concat ss, t2)
                   Just (s,t2) -> (s : concat ss, t2)
            where
                (cs, gen) = uniplate t
                (ss, css) = unzip $ map f cs
                t2 = gen css
