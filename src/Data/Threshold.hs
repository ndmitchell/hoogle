-- Currently unused

module Data.Threshold where

import qualified Data.Heap as Heap

data Threshold k v = Result k v | Threshold k
                     deriving Show


threshold :: Ord k => [Threshold k v] -> [v]
threshold = f Heap.empty
    where
        f h [] = Heap.elems h
        f h (Threshold k:xs) = as ++ f h2 xs
            where (as,h2) = Heap.popUntil k h
        f h (Result k v:xs) = f (Heap.push k v h) xs

