
module Hoogle.DataBase.Texts(Texts, createTexts, searchTexts) where

import Hoogle.Item.All
import General.All

import Data.Binary.Defer
import Data.Binary.Defer.List
import Data.List
import Data.Char


data Texts = Texts (ListDefer ItemId) Trie
             deriving Show

data Trie = Trie {mapping :: [(Char,Defer Trie)], start :: Int, len :: Int}
            deriving Show


instance BinaryDefer Texts where
    bothDefer = defer [\ ~(Texts a b) -> unit Texts << a << b]

instance BinaryDefer Trie where
    bothDefer = defer [\ ~(Trie a b c) -> unit Trie << a << b << c]


{-
TRIE data structure

Given the functions "map" and "pm" we would generate:

"ap"    [map]
"m"     [pm]
"map"   [map]
"p"     [map]
"pm"    [pm]

[item] is the id of the item.

Note the list has been sorted, and all prefixes appear.
-}


createTexts :: [Item] -> Texts
createTexts items = Texts (newListDefer $ map (snd . snd) xs) (f xs)
    where
        xs :: [(Int,(String,ItemId))]
        xs = zip [0..] $ sort [(y, itemId x) | x <- items,
                    y <- init $ tails $ map toLower $ itemName x]

        text = fst . snd
        tailText (a,(_:b,c)) = (a,(b,c))

        f [] = Trie [] 0 0
        f xs = Trie inner (fst $ head xs) (length xs)
            where inner = map g $ groupBy ((==) `on` (head . text)) $
                          dropWhile (null . text) xs
        
        g xs = (head $ text $ head xs, Defer $ f $ map tailText xs)



searchTexts :: Texts -> String -> [ItemId]
searchTexts (Texts list t) x = f t x
    where
        f t ""     = readListDefer list (start t) (len t)
        f t (x:xs) = case lookup (toLower x) (mapping t) of
                          Nothing -> []
                          Just t2 -> f (fromDefer t2) xs
