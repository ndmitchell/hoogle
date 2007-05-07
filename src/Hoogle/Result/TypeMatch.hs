
module Hoogle.Result.TypeMatch(
    TypeMatch(..), TypeDiff(..),
    TypeScore, typeScore
    ) where

import Hoogle.Item.All


data TypeMatch = TypeMatch [Int] [TypeDiff]
                 deriving Show

data TypeDiff = TypeAlpha String String
                deriving (Show, Eq)


type TypeScore = Int

typeScore :: Item -> TypeMatch -> TypeScore
typeScore _ (TypeMatch _ xs) = length xs
