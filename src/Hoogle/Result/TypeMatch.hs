
module Hoogle.Result.TypeMatch(
    TypeMatch(..), TypeDiff(..),
    TypeScore, typeScore, verboseTypeMatch
    ) where

import Hoogle.Item.All


data TypeMatch = TypeMatch [Int] [TypeDiff]
                 deriving Show

data TypeDiff = TypeAlpha String String
              | TypeAlias String
                deriving (Show, Eq)


type TypeScore = Int

typeScore :: Item -> TypeMatch -> TypeScore
typeScore _ (TypeMatch _ xs) = length xs


verboseTypeMatch :: TypeMatch -> String
verboseTypeMatch (TypeMatch _ s) = unwords $ map f s
    where
        f (TypeAlpha x y) = "alpha(" ++ x ++ "," ++ y ++ ")"
        f (TypeAlias x) = "alias(" ++ x ++ ")"
