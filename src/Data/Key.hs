
module Data.Key(
    Key, toKey, fromKey, sortKeys
    ) where

import Data.List


data Key k v = Key k v


instance Eq k => Eq (Key k v) where
    Key k1 v1 == Key k2 v2 = k1 == k2

instance Ord k => Ord (Key k v) where
    compare (Key k1 v1) (Key k2 v2) = compare k1 k2


toKey :: (v -> k) -> v -> Key k v
toKey f v = Key (f v) v


fromKey :: Key k v -> v
fromKey (Key k v) = v


sortKeys :: Ord k => (v -> k) -> [v] -> [v]
sortKeys f = map fromKey . sort . map (toKey f)
