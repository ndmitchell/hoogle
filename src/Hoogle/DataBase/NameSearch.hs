
module Hoogle.DataBase.NameSearch
    (NameSearch, createNameSearch
    ,TextScore, searchNameSearch
    ,completionsNameSearch
    ) where

import Data.Binary.Defer
import Data.Binary.Defer.Array
import Data.Binary.Defer.Index
import qualified Data.Map as Map
import Data.Range
import General.Code
import Hoogle.Item.All


---------------------------------------------------------------------
-- DATA TYPES

{-
The idea is that NameItem's are sorted by name, so exact/start matching
is done by binary searching this list.

The rest of the results are taken by unioning all the suggestions in the
second element, and searching in order. All the results will end up
sorted by name (since they have identical names)

The original code was based around a Trie, gave fast performance, but
didn't merge common strings and consumed about 10x the disk space.
-}


data NameSearch = NameSearch (Array NameItem) [(Char, IntList)]

data NameItem = NameItem {key :: String
                         ,rest :: Defer [(String, [Link Entry])]}


instance Show NameSearch where
    show (NameSearch a b) =
            concat (zipWith (\a b -> show a ++ " " ++ show b) [0..] (elems a)) ++
            unlines [c : " = " ++ show d | (c,d) <- b]

instance Show NameItem where
    show (NameItem a b) = unlines $ a : map f (fromDefer b)
        where f (a,b) = unwords $ " " : a : ['#' : show (linkKey x) | x <- b]

instance BinaryDefer NameSearch where
    put (NameSearch a b) = put2 a b
    get = get2 NameSearch

instance BinaryDefer NameItem where
    put (NameItem a b) = put2 a b
    get = get2 NameItem


---------------------------------------------------------------------
-- CREATION

createNameSearch :: [Link Entry] -> NameSearch
createNameSearch xs = NameSearch (array $ Map.elems items) (Map.toList shortcuts)
    where
        items = buildItems xs
        shortcuts = buildShortcuts items


buildShortcuts :: Map.Map String NameItem -> Map.Map Char IntList
buildShortcuts = Map.map (toIntList . sort) . foldl' add Map.empty . zip [0..] . Map.keys
    where
        add mp (i,s) = foldl' g mp $ nub s
            where g mp x = Map.insertWith (++) x [i] mp


buildItems :: [Link Entry] -> Map.Map String NameItem
buildItems = Map.map norm . foldl' add Map.empty
    where
        add mp e = Map.insertWith f ltext (NameItem ltext $ Defer [(text, [e])]) mp
            where
                text = entryName $ fromLink e
                ltext = map toLower text

                f _ (NameItem a b) = NameItem a $ Defer $ g $ fromDefer b
                g [] = [(text, [e])]
                g ((x1,x2):xs) | x1 == text = (x1, e : x2) : xs
                               | otherwise = (x1,x2) : g xs

        norm (NameItem a b) = NameItem a $ Defer $ f $ fromDefer b
            where f x = sortFst [(a, sortOn linkKey b) | (a,b) <- x]


---------------------------------------------------------------------
-- SEARCHING

-- lower is better
data TextScore = TSExact | TSStart | TSNone
                 deriving (Eq,Ord)

instance Show TextScore where
    show TSExact = "exact"
    show TSStart = "start"
    show TSNone = "_"


{-
Step 1: Binary search for find the exact match
Step 2: Follow from that item finding ones which start
Step 3: Use the hint set to merge into a list of results
-}

searchNameSearch :: NameSearch -> String -> [(Link Entry,EntryView,TextScore)]
searchNameSearch (NameSearch items shortcuts) str = step1 ++ step2 ++ step3
    where
        lstr = map toLower str
        nstr = length str
        rangePrefix = FocusOn $ rangeStartCount 0 nstr

        (exact,prefix) = startPos items lstr
        (prefixes,lastpre) = followPrefixes items lstr prefix


        step1 = if isJust exact then f TSExact yes ++ f TSStart no else []
            where
                (yes,no) = partition ((==) str . fst) $ fromDefer $ rest $ items ! fromJust exact
                f scr xs = [(x, rangePrefix, scr) | x <- concatMap snd xs]

        step2 = [(x, rangePrefix, TSStart) | x <- prefixes]

        seen i = fromMaybe prefix exact <= i && i <= lastpre
        step3 = [(e,view,TSNone) | i <- xs, let x = items ! i
                , Just p <- [testMatch lstr $ key x]
                , let view = FocusOn $ rangeStartCount p nstr
                , e <- concatMap snd $ fromDefer $ rest x]
            where xs = filter (not . seen) $ intersectOrds $
                       map (maybe [] fromIntList . flip lookup shortcuts) $ nub lstr


-- Return the index of the string as the first component
-- Return the first possible index of the prefix as the second
startPos :: Array NameItem -> String -> (Maybe Int, Int)
startPos xs x = f 0 (arraySize xs - 1)
    where
        f low high | high - low < 3 = g low high
                   | otherwise =
            case compare x (key $ xs ! mid) of
                    EQ -> (Just mid, mid+1)
                    GT -> f (mid+1) high
                    LT -> f low (mid-1)
            where
                mid = (high + low) `div` 2

        g low high | low > high = (Nothing, low)
        g low high = if k == x then (Just low, low+1)
                     else if x `isPrefixOf` k then (Nothing, low)
                     else g (low+1) high
            where k = key $ xs ! low


-- Return all the items you can match following the prefix
-- Plus the last item that was a valid prefix index
followPrefixes :: Array NameItem -> String -> Int -> ([Link Entry], Int)
followPrefixes xs x i = f i
    where
        n = arraySize xs
        f i | i < n && x `isPrefixOf` key xsi = (concatMap snd (fromDefer $ rest xsi) ++ res, end)
            | otherwise = ([],i-1)
            where xsi = xs ! i
                  (res,end) = f (i+1)


testMatch :: String -> String -> Maybe Int
testMatch find within = listToMaybe [i | (i,x) <- zip [0..] $ tails within, find `isPrefixOf` x]


intersectOrd :: [Int] -> [Int] -> [Int]
intersectOrd (x:xs) (y:ys) = case compare x y of
    EQ -> x : intersectOrd xs ys
    LT -> intersectOrd xs (y:ys)
    GT -> intersectOrd (x:xs) ys
intersectOrd _ _ = []


intersectOrds :: [[Int]] -> [Int]
intersectOrds = fold1 intersectOrd


---------------------------------------------------------------------
-- COMPLETIONS

completionsNameSearch :: NameSearch -> String -> [String]
completionsNameSearch (NameSearch items _) str =
        concatMap (map fst . fromDefer . rest) $
        takeWhile ((lstr `isPrefixOf`) . key) $
        map ((!) items) [start .. arraySize items - 1]
    where
        lstr = map toLower str
        nstr = length str

        (exact,prefix) = startPos items lstr
        start = fromMaybe prefix exact


---------------------------------------------------------------------
-- IntList TYPE

type IntList = [IntRange]
data IntRange = IntRange !Int !Int

instance Show IntRange where
    show (IntRange a b) = show a ++ ".." ++ show b

instance BinaryDefer IntRange where
    put (IntRange a b) = put2 a b
    get = get2 IntRange


toIntList :: [Int] -> IntList
toIntList [] = []
toIntList (x:xs) = f x xs
    where
        f i [] = [IntRange x i]
        f i (y:ys) | y == i+1 = f y ys
                   | otherwise = IntRange x i : toIntList (y:ys)


fromIntList :: IntList -> [Int]
fromIntList = concatMap (\(IntRange a b) -> [a..b])


