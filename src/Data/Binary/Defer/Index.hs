
module Data.Binary.Defer.Index(
    Id,
    Index, newIndex,
    Lookup, newLookup, lookupKey, lookupIndex
    ) where

import Data.Binary.Defer
import Data.Binary.Defer.Array

type Id = Int

newtype Index a = Index (Array a)

-- | Items will obtain the Id's 0..length-1
newIndex :: [a] -> Index a
newIndex = Index . array


newtype Lookup a = Lookup {lookupKey :: Id}

newLookup :: Id -> Lookup a
newLookup i = Lookup i

lookupIndex :: Lookup a -> Index a -> a
lookupIndex (Lookup i) (Index xs) = xs ! i


instance BinaryDefer a => BinaryDefer (Index a) where
    put (Index x) = put x
    get = get1 Index

instance BinaryDefer (Lookup a) where
    put (Lookup key) = put key
    get = get1 Lookup
    size _ = size (undefined :: Id)
    putFixed (Lookup key) = putFixed key
    getFixed = getFixed1 Lookup


instance Show a => Show (Index a) where
    show (Index xs) = unlines $ map show $ elems xs

instance Show (Lookup a) where
    show (Lookup key) = "#" ++ show key

instance Functor Index where
    fmap f (Index x) = Index $ fmap f x
