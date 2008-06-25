{- TODO:
    Make the Trie (Int,Int,Int)
    Have from,to-exact,to-prefix
    Can then only check from..to-exact for exact matches
    And use the Chunk (Int,_) to decide which start at 0
    Therefore don't need to do much sorting

    Also make EntryId be the search key of an element,
    then can order elements from the same database
    without looking at the Entry (other than checking
    for exact-string property)
-}

module Hoogle.DataBase.NameSearch
    (NameSearch, createNameSearch
    ,TextScore, searchNameSearch
    ) where

import Data.Binary.Defer
import Data.Binary.Defer.Trie
import Data.Binary.Defer.Chunk
import Data.Binary.Defer.Index
import Data.Char
import Data.List
import Data.Range
import General.Code
import Hoogle.DataBase.Item
import Hoogle.TextBase.All


---------------------------------------------------------------------
-- DATA TYPES

data NameSearch = NameSearch (Trie (Int,Int)) (Chunk (Int,Lookup Entry))
                  deriving Show

instance BinaryDefer NameSearch where
    put (NameSearch a b) = put a >> put b
    get = get2 NameSearch

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

createNameSearch :: [(TextItem, Maybe Entry)] -> NameSearch
createNameSearch xs = NameSearch
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

-- lower is better
data TextScore = TSExact | TSStart | TSNone
                 deriving (Eq,Ord)

instance Show TextScore where
    show TSExact = "exact"
    show TSStart = "start"
    show TSNone = "_"


searchNameSearch :: NameSearch -> Index Entry -> String -> [(Entry,EntryView,TextScore)]
searchNameSearch (NameSearch trie chunk) ents str =
    case lookupTrie (map toLower str) trie of
        Nothing -> []
        Just (i,j) -> [(ent, FocusOn (p,p+nstr-1), score p ent)
                      |(p,e) <- lookupChunk (rangeStartEnd i j) chunk
                      ,let ent = lookupIndex e ents]
    where
        nstr = length str
        score p ent | p == 0 = if entryName ent == str then TSExact else TSStart
                    | otherwise = TSNone
