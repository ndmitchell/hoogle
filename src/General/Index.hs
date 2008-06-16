
module General.Index(
    Id,
    Index, newIndex,
    Lookup, newLookup, lookupKey, lookupVal
    ) where

import Data.Array
import Data.Binary.Defer

type Id = Int

newtype Index a = Index (Array Id a)

-- | Items will obtain the Id's 0..length-1
newIndex :: [a] -> Index a
newIndex xs = Index $ listArray (0,length xs - 1) xs


data Lookup a = Lookup {lookupKey :: Id, lookupVal :: a}

newLookup :: Id -> Index a -> Lookup a
newLookup i (Index xs) = Lookup i (xs ! i)


instance BinaryDefer (Index a)
instance BinaryDefer (Lookup a)


instance Show a => Show (Index a) where
    show (Index xs) = unlines $ map show $ elems xs

instance Show (Lookup a) where
    show (Lookup key _) = "#" ++ show key
