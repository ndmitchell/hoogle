
module Data.IntHeap where

import qualified Data.Map as Map

-- NOTE: Horribly inefficient
-- stored in order
newtype IntHeap v = IntHeap [(Int,v)]

-- should be inserting elements 


-- insert a value with a cost, does NOT overwrite values
push :: Int -> v -> IntHeap v -> IntHeap v
push k v (IntHeap xs) = IntHeap $ f xs
    where
        f ((a,b):xs) | k > a = (a,b) : f xs
        f xs = (k,v):xs

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
