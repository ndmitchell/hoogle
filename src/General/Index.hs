
module General.Index(
    Index, newIndex,
    Lookup, newLookup, lookupKey, lookupVal
    ) where

import Data.Array


newtype Index a = Index (Array Int a)

newIndex :: [a] -> Index a
newIndex xs = Index $ listArray (0,length xs - 1) xs


data Lookup a = Lookup {lookupKey :: Int, lookupVal :: a}

newLookup :: Int -> Index a -> Lookup a
newLookup i (Index xs) = Lookup i (xs ! i)
