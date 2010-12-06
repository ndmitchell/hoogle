
module Data.Key(
    Key, keyPair, toKey, fromKey,
    sortKeys, sortWith
    ) where

import Data.List


data Key k v = Key k v


instance Eq k => Eq (Key k v) where
    Key k1 v1 == Key k2 v2 = k1 == k2

instance Ord k => Ord (Key k v) where
    compare (Key k1 v1) (Key k2 v2) = compare k1 k2


keyPair :: k -> v -> Key k v
keyPair = Key


toKey :: (v -> k) -> v -> Key k v
toKey f v = Key (f v) v


fromKey :: Key k v -> v
fromKey (Key k v) = v


sortKeys :: Ord k => [Key k v] -> [v]
sortKeys = map fromKey . sort


-- | @sort f == sortBy (comparing f)@
--   but @f@ will only be applied to each element once
sortWith :: Ord k => (v -> k) -> [v] -> [v]
sortWith f = sortKeys . map (toKey f)
