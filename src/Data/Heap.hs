
module Data.Heap(
    Heap, empty,
    toList,
    singleton,
    push, pushList,
    pop, popUntil
    ) where

import Prelude
import qualified Data.Map as Map
import Data.Maybe
import Data.List as List


-- NOTE: Horribly inefficient
-- stored in order
newtype Heap k v = Heap [(k,v)]

empty :: Heap k v
empty = Heap []


fromList :: Ord k => [(k,v)] -> Heap k v
fromList xs = pushList xs empty


toList :: Heap k v -> [(k,v)]
toList (Heap xs) = xs


elems :: Heap k v -> [v]
elems = map snd . toList


singleton :: k -> v -> Heap k v
singleton k v = Heap [(k,v)]


-- insert a value with a cost, does NOT overwrite values
push :: Ord k => k -> v -> Heap k v -> Heap k v
push k v (Heap xs) = Heap $ f xs
    where
        f ((a,b):xs) | k > a = (a,b) : f xs
        f xs = (k,v):xs


pushList :: Ord k => [(k,v)] -> Heap k v -> Heap k v
pushList xs mp = foldr (uncurry push) mp xs


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
