
module General.Heap(
    Heap, empty,
    fromList, toList, elems,
    singleton,
    insert, insertList,
    pop, popUntil, popWhile
    ) where

import Prelude
import qualified Data.Map as Map


-- (k,v) pairs are stored in reverse order

newtype Heap k v = Heap (Map.Map k [(k,v)])

empty :: Heap k v
empty = Heap Map.empty


fromList :: Ord k => [(k,v)] -> Heap k v
fromList xs = insertList xs empty


toList :: Heap k v -> [(k,v)]
toList (Heap mp) = concatMap reverse $ Map.elems mp


elems :: Heap k v -> [v]
elems (Heap mp) = concatMap (reverse . map snd) $ Map.elems mp


singleton :: Ord k => k -> v -> Heap k v
singleton k v = insert k v empty


-- insert a value with a cost, does NOT overwrite values
insert :: Ord k => k -> v -> Heap k v -> Heap k v
insert k v (Heap xs) = Heap $ Map.insertWith (++) k [(k,v)] xs


insertList :: Ord k => [(k,v)] -> Heap k v -> Heap k v
insertList xs mp = foldr (uncurry insert) mp xs


-- retrieve the lowest value (can use minView in the future)
-- does NOT guarantee to be the first one inserted at that level
pop :: Ord k => Heap k v -> Maybe ((k,v), Heap k v)
pop (Heap mp) | Map.null mp = Nothing
              | null kvs    = Just ((k1,v1), Heap mp2)
              | otherwise   = Just ((k1,v1), Heap $ Map.insert k kvs mp2)
    where
        ((k,(k1,v1):kvs),mp2) = Map.deleteFindMin mp


-- until you reach this key, do not pop those at this key
-- guarantees to return by order, then insertion time
popUntil :: Ord k => k -> Heap k v -> ([v], Heap k v)
popUntil x = popBy (< x)


-- until you reach this key, and then pop those at this key
-- guarantees to return by order, then insertion time
popWhile :: Ord k => k -> Heap k v -> ([v], Heap k v)
popWhile x = popBy (<= x)


popBy :: Ord k => (k -> Bool) -> Heap k v -> ([v], Heap k v)
popBy cmp (Heap mp)
    | Map.null mp || not (cmp k) = ([], Heap mp)
    | otherwise = (reverse (map snd kvs) ++ res, mp3)
        where
            ((k,kvs),mp2) = Map.deleteFindMin mp
            (res,mp3) = popBy cmp (Heap mp2)
