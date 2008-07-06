
module Data.IntHeap where

import Prelude hiding (min)
import qualified Data.Map as Map
import Data.Maybe


-- NOTE: Horribly inefficient
-- stored in order
newtype IntHeap v = IntHeap [(Int,v)]

empty :: IntHeap v
empty = IntHeap []


-- insert a value with a cost, does NOT overwrite values
push :: Int -> v -> IntHeap v -> IntHeap v
push k v (IntHeap xs) = IntHeap $ f xs
    where
        f ((a,b):xs) | k > a = (a,b) : f xs
        f xs = (k,v):xs


pushList :: [(Int,v)] -> IntHeap v -> IntHeap v
pushList xs mp = foldr (uncurry push) mp xs


-- retrieve the lowest value
pop :: IntHeap v -> Maybe ((Int,v), IntHeap v)
pop (IntHeap []) = Nothing
pop (IntHeap (x:xs)) = Just (x, IntHeap xs)


-- until you reach this key, do not pop those at this key
-- i.e. (<), not (<=)
-- guarantees to return the lowest first
popUntil :: Int -> IntHeap v -> ([v], IntHeap v)
popUntil i (IntHeap xs) = (map snd a, IntHeap b)
    where (a,b) = span ((< i) . fst) xs


min :: IntHeap v -> Maybe Int
min (IntHeap xs) = listToMaybe $ map fst xs
