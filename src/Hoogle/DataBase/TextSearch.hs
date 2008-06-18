
module Hoogle.DataBase.TextSearch
    (TextSearch, createTextSearch
    ,TextScore, searchTextSearch
    ) where

import Data.Binary.Defer
import Data.Binary.Defer.Trie
import Data.Binary.Defer.Chunk
import Data.Binary.Defer.Index
import Data.Char
import Data.List
import General.Code
import Hoogle.DataBase.Item
import Hoogle.TextBase.All


---------------------------------------------------------------------
-- DATA TYPES

data TextSearch = TextSearch (Trie (Int,Int)) (Chunk (Int,Lookup Entry))
                  deriving Show

instance BinaryDefer TextSearch where
    put (TextSearch a b) = put a >> put b
    get = get2 TextSearch

{-
TRIE data structure

Given the functions "map" and "pm" we would generate:

Trie:
"ap"   (0,0)
"m"    (1,2)
"ma"   (2,2)  
"map"  (2,2)
"p"    (3,4)
"pm"   (4,4)

Chunk:
0      (1,[map])
1      (1,[pm])
2      (0,[map])
3      (2,[map])
4      (0,[pm])

There will be one trie entry per unique substring.
There will be one chunk entry per prefix.
Both are sorted by the string they represent.

[item] is the id of the item.

Given: x (from,to) (start,id)

All items in the list ['from'..'to'] have the string 'x'
starting at position 'start' in the name given by 'id'.
-}

---------------------------------------------------------------------
-- CREATION

createTextSearch :: [(TextItem, Maybe Entry)] -> TextSearch
createTextSearch xs = TextSearch
        (newTrie $ f sub (zip [0..] pre))
        (newChunk $ map snd pre)
    where
        ys = extractText xs
        sub = map head $ group $ sort $ concatMap (substrs . fst) ys
        pre = sortBy (compare `on` fst)
                  [(p,(i,e)) | (s,e) <- ys, (i,p) <- zip [0..] $ prefixes s]

        f :: [String] -> [(Int,(String,a))] -> [(String,(Int,Int))]
        f [] _ = []
        f (x:xs) ys = (x,(s,s+n-1)) : f xs ys2
            where
                s = fst $ head ys
                n = length eq + length pr
                (eq,ys2) = span ((==) x . fst . snd) ys
                pr = takeWhile (isPrefixOf x . fst . snd) ys2


extractText :: [(TextItem, Maybe Entry)] -> [(String, Lookup Entry)]
extractText xs = [(map toLower s, newLookup $ entryId e)
                 |(_, Just e) <- xs, Focus s <- entryText e]


substrs, prefixes :: [a] -> [[a]]
substrs = concatMap (tail . inits) . prefixes
prefixes = init . tails


---------------------------------------------------------------------
-- SEARCHING

data TextScore = TextScore
                 deriving (Eq,Ord,Show)

searchTextSearch :: TextSearch -> Index Entry -> String -> [(Entry,EntryView,TextScore)]
searchTextSearch (TextSearch trie chunk) ents str =
    case lookupTrie str trie of
        Nothing -> []
        Just i -> [(lookupIndex e ents, FocusOn (p,nstr), TextScore)
                  |(p,e) <- lookupChunk i chunk]
    where
        nstr = length str
