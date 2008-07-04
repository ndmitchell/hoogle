
module Data.Binary.Defer.Index(
    Id,
    Index, newIndex,
    Lookup, newLookup, lookupKey, lookupIndex,
    IndexMutable, newIndexMutable, getIndex, indexFreeze
    ) where

import General.Util
import Data.Binary.Defer
import Data.Binary.Defer.Array
import qualified Data.Map as Map
import Data.Maybe
import Data.List

type Id = Int


---------------------------------------------------------------------
-- INDEX

newtype Index a = Index (Array a)


-- | Items will obtain the Id's 0..length-1
newIndex :: [a] -> Index a
newIndex = Index . array


instance BinaryDefer a => BinaryDefer (Index a) where
    put (Index x) = put x
    get = get1 Index

instance Show a => Show (Index a) where
    show (Index xs) = unlines $ map show $ elems xs

instance Functor Index where
    fmap f (Index x) = Index $ fmap f x


---------------------------------------------------------------------
-- LOOKUP

newtype Lookup a = Lookup {lookupKey :: Id}
                   deriving (Eq,Ord)

newLookup :: Id -> Lookup a
newLookup = Lookup

lookupIndex :: Lookup a -> Index a -> a
lookupIndex (Lookup i) (Index xs) = xs ! i


instance BinaryDefer (Lookup a) where
    put (Lookup key) = put key
    get = get1 Lookup
    size _ = size (undefined :: Id)
    putFixed (Lookup key) = putFixed key
    getFixed = getFixed1 Lookup


instance Show (Lookup a) where
    show (Lookup key) = "#" ++ show key


---------------------------------------------------------------------
-- INDEXMUTABLE

newtype IndexMutable a = IndexMutable (Map.Map a Id)


newIndexMutable :: IndexMutable a
newIndexMutable = IndexMutable Map.empty


getIndex :: Ord a => a -> IndexMutable a -> (IndexMutable a, Lookup a)
getIndex x (IndexMutable mp) = (IndexMutable mp2, Lookup $ fromMaybe n res)
    where (res,mp2) = Map.insertLookupWithKey (\_ _ a -> a) x n mp
          n = Map.size mp


indexFreeze :: IndexMutable a -> Index a
indexFreeze (IndexMutable mp) = newIndex $ map fst $ sortBy (compare `on` snd) $ Map.toList mp
