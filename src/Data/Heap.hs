
module Data.Heap(
    Heap, empty,
    fromList, toList,
    singleton,
    insert, insertList,
    pop, popUntil
    ) where

import Prelude
import qualified Data.Map as Map
import Data.Maybe


-- NOTE: Horribly inefficient
-- stored in order
newtype Heap k v = Heap [(k,v)]

empty :: Heap k v
empty = Heap []


fromList :: Ord k => [(k,v)] -> Heap k v
fromList xs = insertList xs empty


toList :: Heap k v -> [(k,v)]
toList (Heap xs) = xs


singleton :: Ord k => k -> v -> Heap k v
singleton k v = insert k v empty


-- insert a value with a cost, does NOT overwrite values
insert :: Ord k => k -> v -> Heap k v -> Heap k v
insert k v (Heap xs) = Heap $ f xs
    where
        f ((a,b):xs) | k > a = (a,b) : f xs
        f xs = (k,v):xs


insertList :: Ord k => [(k,v)] -> Heap k v -> Heap k v
insertList xs mp = foldr (uncurry insert) mp xs


-- retrieve the lowest value
pop :: Heap k v -> Maybe ((k,v), Heap k v)
pop (Heap []) = Nothing
pop (Heap (x:xs)) = Just (x, Heap xs)


-- until you reach this key, do not pop those at this key
-- i.e. (<), not (<=)
-- guarantees to return the lowest first
popUntil :: Ord k => k -> Heap k v -> ([v], Heap k v)
popUntil i (Heap xs) = (map snd a, Heap b)
    where (a,b) = span ((< i) . fst) xs
