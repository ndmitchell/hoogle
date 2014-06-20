
-- | Like Substring but for whole words, used for documentation.
--   Just sort all the words, and do a binary search.
module Search.Type(
    Key(..), Package(..),
    Stm(..), Typ(..)
    ) where

import Data.Word


-- | FIXME: Should be Word32 with a global offset table, once optimised
data Key = Key String deriving (Show,Read)

data Package
    = Package String
    | Category String
    | Author String

instance Show Package where
    show (Package x) = "package-" ++ x
    show (Category x) = "category-" ++ x
    show (Author x) = "author-" ++ x


data Stm = TypeAlias Typ Typ
         | Instance Typ
         | Function [Typ] Typ

data Typ = TCon String
         | TVar String
         | TApp Typ [Typ]

