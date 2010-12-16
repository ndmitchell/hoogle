{-# LANGUAGE DeriveDataTypeable #-}

module Data.Binary.Defer.Index(
    Id,
    Index, newIndex,
    Link, newLink, fromLink, linkKey, indexLinks
    ) where

import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import Data.Binary.Defer
import Data.Binary.Defer.Array
import Data.Ord
import Data.Typeable

type Id = Int


---------------------------------------------------------------------
-- INDEX

newtype Index a = Index (Array a)
                  deriving Typeable


-- | Items will obtain the Id's 0..length-1
newIndex :: [a] -> Index a
newIndex = Index . array


instance (Typeable a, BinaryDefer a) => BinaryDefer (Index a) where
    put (Index x) = put x
    get = do res <- get1 Index; getDeferPut res; return res

instance Show a => Show (Index a) where
    show (Index xs) = unlines $ zipWith f [0..] (elems xs)
        where
            f i x = "#" ++ si ++ replicate (width - length si + 1) ' ' ++ show x
                where si = show i
            width = length $ show $ arraySize xs


---------------------------------------------------------------------
-- LINK

data Link a = Link Id a

newLink :: Id -> a -> Link a
newLink = Link

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


instance Bin.Binary (Link a) where
    put = Bin.putWord32host . fromIntegral . linkKey
    get = error "Can't implement Data.Binary.Get on Link"

instance Typeable a => BinaryDeferGet (Link a) where
    binaryDeferGet = do
        Index xs <- getDeferGet
        return $ do
            i <- fmap fromIntegral Bin.getWord32host
            return $ Link i $ xs ! i

instance FixedBinary (Link a) where
    fixedSize _ = 4

indexLinks :: Index a -> [Link a]
indexLinks (Index x) = zipWith newLink [0..] $ elems x
