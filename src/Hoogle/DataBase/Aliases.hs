
module Hoogle.DataBase.Aliases(
    Aliases, createAliases, followAlias
    ) where


import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import qualified Data.Map as Map
import Data.Binary.Defer

newtype Aliases = Aliases (Map.Map String Alias)
                  deriving Show

instance BinaryDefer Aliases where
    put = undefined
    get = undefined


data Alias = Alias
    {name :: String -- the name of the Alias
    ,args :: [String] -- the arguments to the type
    ,free :: [String] -- free variables on the RHS but not LHS
    ,rhs :: TypeSig -- the resulting type
    ,forward :: Bool -- is it type, or the reverse
    } deriving Show


createAliases :: [TextItem] -> Aliases
createAliases _ = Aliases Map.empty



-- these are all the one step alias followings
-- [String] is free variables in and out
-- does a deep search for aliases
followAlias :: TypeSig -> [String] -> [([String],TypeSig)]
followAlias _ _ = []


