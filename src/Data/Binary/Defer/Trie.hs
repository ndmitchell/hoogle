
module Data.Binary.Defer.Trie(
    Trie, newTrie, lookupTrie
    ) where

import Data.Binary.Defer
import Data.List
import Data.Maybe
import General.Code


-- Keep simple first, can move to an Array later if necessary
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


instance BinaryDefer a => BinaryDefer (Trie a) where
    put (Trie a b) = putDefer $ put2 a b
    get = getDefer $ get2 Trie

instance Show a => Show (Trie a) where
    show = unlines . f ""
        where
            f str (Trie res xs) =
                [show str ++ " = " ++ show r | Just r <- [res]] ++
                concat [f (str ++ [c]) t | (c,t) <- xs]
