
-- | Like Substring but for whole words, used for documentation.
--   Just sort all the words, and do a binary search.
module Search.Type(
    Key(..), Package(..),
    Stm(..), Typ(..)
    ) where

import Data.Word


newtype Key = Key Word32

newtype Package = Package String


data Stm = TypeAlias Typ Typ
         | Instance Typ
         | Function [Typ] Typ

data Typ = TCon String
         | TVar String
         | TApp Typ [Typ]
