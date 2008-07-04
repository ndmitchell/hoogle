
module Hoogle.DataBase.Aliases(
    Aliases, createAliases, followAlias
    ) where


import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import qualified Data.Binary.Defer.Map as Map
import Data.Binary.Defer

newtype Aliases = Aliases (Map.Map String Alias)
                  deriving Show

instance BinaryDefer Aliases where
    put (Aliases a) = put a
    get = get1 Aliases


data Alias = Alias
    {name :: String -- the name of the Alias
    ,args :: [String] -- the free variables
    ,rhs :: Type -- the resulting type
    } deriving Show

instance BinaryDefer Alias where
    put (Alias a b c) = put3 a b c
    get = get3 Alias


createAliases :: [TextItem] -> Aliases
createAliases ti = Aliases $ Map.fromList
    [ (name, Alias name [v | TVar v <- args] rhs)
    | ItemAlias (TypeSig _ lhs) (TypeSig _ rhs) <- ti
    , let (TLit name, args) = fromTApp lhs]


-- these are all the one step alias followings
-- [String] is free variables in and out
-- does a deep search for aliases
followAlias :: TypeSig -> [TypeSig]
followAlias _ = []


