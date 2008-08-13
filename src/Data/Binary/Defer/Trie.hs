
module Data.Binary.Defer.Trie(
    Trie, newTrie, lookupTrie, unionsWith
    ) where

import Data.Binary.Defer
import Data.List
import Data.Maybe
import General.Code


-- Keep simple first, can move to an Array later if necessary
-- the second list is always ordered
data Trie a = Trie (Maybe a) [(Char,Trie a)]
              deriving Eq


newTrie :: [(String,a)] -> Trie a
newTrie = newTrieOrdered . sortBy (compare `on` fst)


newTrieOrdered :: [(String,a)] -> Trie a
newTrieOrdered xs = Trie
        (fmap snd $ listToMaybe as)
        (map f $ groupBy ((==) `on` (head . fst)) bs)
    where
        (as,bs) = span (null . fst) xs
        f xs = (head $ fst $ head xs, newTrieOrdered [(a,b) | (_:a,b) <- xs])


lookupTrie :: String -> Trie a -> Maybe a
lookupTrie [] (Trie a b) = a
lookupTrie (x:xs) (Trie a b) = lookup x b >>= lookupTrie xs


unionsWith :: (a -> a -> a) -> [Trie a] -> Trie a
unionsWith f [] = newTrie []
unionsWith f [x] = x
unionsWith f (x:xs) = foldl (unionWith f) x xs


unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith merge (Trie x xs) (Trie y ys) = Trie xy $ f xs ys
    where
        xy = case (x,y) of
                  (Just x, Nothing) -> Just x
                  (Nothing, Just y) -> Just y
                  (Just x,  Just y) -> Just $ merge x y
                  (Nothing,Nothing) -> Nothing

        f ((x1,x2):xs) ((y1,y2):ys) = case compare x1 y1 of
            EQ -> (x1,unionWith merge x2 y2) : f xs ys
            LT -> (x1,x2) : f xs ((y1,y2):ys)
            GT -> (y1,y2) : f ((x1,x2):xs) ys
        f xs [] = xs
        f [] ys = ys


instance BinaryDefer a => BinaryDefer (Trie a) where
    put (Trie a b) = putDefer $ put2 a b
    get = getDefer $ get2 Trie

instance Show a => Show (Trie a) where
    show = unlines . f ""
        where
            f str (Trie res xs) =
                [show str ++ " = " ++ show r | Just r <- [res]] ++
                concat [f (str ++ [c]) t | (c,t) <- xs]
