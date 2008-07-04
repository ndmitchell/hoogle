
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
                  deriving Show

instance BinaryDefer Aliases where
    put (Aliases a) = put a
    get = get1 Aliases


data Alias = Alias
    {args :: [String] -- the free variables
    ,rhs :: Type -- the resulting type
    } deriving Show

instance BinaryDefer Alias where
    put (Alias a b) = put2 a b
    get = get2 Alias


createAliases :: [TextItem] -> Aliases
createAliases ti = Aliases $ Map.fromList
    [ (name, Alias [v | TVar v <- args] rhs)
    | ItemAlias (TypeSig _ lhs) (TypeSig _ rhs) <- ti
    , let (TLit name, args) = fromTApp lhs]


-- these are all the one step alias followings
-- [String] is free variables in and out
-- does a deep search for aliases
followAliases :: Aliases -> TypeSig -> [TypeSig]
followAliases (Aliases mp) (TypeSig a b) = map (TypeSig a) $ concatMap f $ contexts b
    where
        f (TApp (TLit x) xs, gen) = case Map.lookup x mp of
            Just a@(Alias vs rhs) | length vs == length xs -> [gen $ followAlias xs a]
            Nothing -> []
        f (TLit x, gen) = f (TApp (TLit x) [], gen)
        f _ = []
        

followAlias :: [Type] -> Alias -> Type
followAlias ts (Alias vs rhs) = transform f rhs
    where
        rep = zip vs ts
        f (TVar v) = fromJust $ lookup v rep
        f x = x

