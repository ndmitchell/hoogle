
module Hoogle.Result.TypeMatch(
    TypeMatch(..), TypeDiff(..)
    ) where


data TypeMatch = TypeMatch [TypeDiff]
                 deriving Show

data TypeDiff = TypeAlpha String String
                deriving (Show, Eq)

