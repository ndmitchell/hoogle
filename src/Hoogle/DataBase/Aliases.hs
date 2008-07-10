
module Hoogle.DataBase.Aliases(
    Aliases, createAliases, followAliases
    ) where

import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import qualified Data.Binary.Defer.Map as Map
import Data.Binary.Defer
import Data.Generics.Uniplate
import Data.Maybe


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
createAliases ti = Aliases $ Map.fromList
    [ (name, Alias [v | TVar v <- args] rhs)
    | ItemAlias (TypeSig _ lhs) (TypeSig _ rhs) <- ti
    , let (TLit name, args) = fromTApp lhs]


-- follow an alias at this point
-- these are all the one step alias followings
followAliases :: Aliases -> Type -> Maybe (String,Type)
followAliases (Aliases mp) (TApp (TLit x) xs) = case Map.lookup x mp of
    Just a@(Alias vs rhs) | length vs == length xs -> Just (x, followAlias xs a)
    _ -> Nothing
followAliases as (TLit x) = followAliases as (TApp (TLit x) [])
followAliases as _ = Nothing


followAlias :: [Type] -> Alias -> Type
followAlias ts (Alias vs rhs) = transform f rhs
    where
        rep = zip vs ts
        f (TVar v) = fromJust $ lookup v rep
        f x = x

