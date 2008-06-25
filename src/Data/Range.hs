{-|
    Invariants: Count >= 0, End >= Start, Start >= 0

    Start/End are 0-based indicies

    Count may be maxBound::Int!

    The rangeStartCount and rangeStartEnd methods ensure the invariants,
    modifying the range if necessary
-}

module Data.Range(
    Range,
    rangeStart, rangeCount, rangeEnd,
    rangeStartCount, rangeStartEnd,
    listRange
    ) where

import Data.List
import General.Code


data Range = Range {rangeStart :: Int, rangeCount :: Int}


instance Show Range where
    show r = "Range {rangeStart = " ++ show (rangeStart r) ++
                  ", rangeCount = " ++ show (rangeCount r) ++
                  ", rangeEnd = " ++ show (rangeEnd r) ++ "}"


nat = max (0::Int)


rangeEnd :: Range -> Int
rangeEnd r = rangeStart r + rangeCount r - 1


rangeStartCount :: Int -> Int -> Range
rangeStartCount start count = Range (nat start) (nat count)


rangeStartEnd :: Int -> Int -> Range
rangeStartEnd start end = Range start2 (nat $ end - start2 + 1)
    where start2 = nat start


listRange :: Range -> [a] -> [a]
listRange r = take (rangeCount r) . drop (rangeStart r)


-- make sure in the resultant range
-- end r_{n} < (start r_{n+1} - 1)
mergeRange :: [Range] -> [Range]
mergeRange = foldr f [] . sortBy (compare `on` rangeStart)
    where
        f (Range s1 c1) (Range s2 c2 : rs) | s2 <= s1+c1 = r12 : rs
            where r12 = Range s1 (max c1 (s2-s1 + c2))
        f r rs = [r | rangeCount r > 0] ++ rs
