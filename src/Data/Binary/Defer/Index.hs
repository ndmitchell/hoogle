
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
import Data.Typeable

type Id = Int


---------------------------------------------------------------------
-- INDEX

newtype Index a = Index (Array a)


typename_Index = mkTyCon "Data.Binary.Defer.Index.Index"
instance Typeable1 Index where typeOf1 _ = mkTyConApp typename_Index []
instance Typeable a => Typeable (Index a) where typeOf = typeOfDefault


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
    size _ = size (0 :: Id)
    putFixed (Lookup key) = putFixed key
    getFixed = getFixed1 Lookup


instance Show (Lookup a) where
    show (Lookup key) = "#" ++ show key


---------------------------------------------------------------------
-- LINK

data Link a = Link Id a

fromLink :: Link a -> a
fromLink (Link k v) = v

linkKey :: Link a -> Id
linkKey (Link k v) = k

instance Eq (Link a) where
    a == b = linkKey a == linkKey b

instance Ord a => Ord (Link a) where
    compare a b = compare (fromLink a) (fromLink b)

instance Show a => Show (Link a) where
    show = show . fromLink

instance Typeable a => BinaryDefer (Link a) where
    put = put . linkKey
    get = do
        i <- get
        Index xs <- getDeferGet
        return $ Link i $ xs ! i

    size _ = size (0 :: Id)
    putFixed = put
    getFixed = get


---------------------------------------------------------------------
-- INDEXMUTABLE

newtype IndexMutable a = IndexMutable (Map.Map a Id)

instance Show a => Show (IndexMutable a) where
    show = show . indexFreeze


newIndexMutable :: IndexMutable a
newIndexMutable = IndexMutable Map.empty


getIndex :: Ord a => a -> IndexMutable a -> (IndexMutable a, Lookup a)
getIndex x (IndexMutable mp) = (IndexMutable mp2, Lookup $ fromMaybe n res)
    where (res,mp2) = Map.insertLookupWithKey (\_ _ a -> a) x n mp
          n = Map.size mp


indexFreeze :: IndexMutable a -> Index a
indexFreeze (IndexMutable mp) = newIndex $ map fst $ sortBy (compare `on` snd) $ Map.toList mp
