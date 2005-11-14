{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

{-|
    General utilities
-}
module Hoogle.General where

import Data.List

-- | If anyone of them returns Nothing, the whole thing does
mapMaybeAll :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeAll f xs = g [] xs
    where
        g acc [] = Just (reverse acc)
        g acc (x:xs) = case f x of
                           Just a -> g (a:acc) xs
                           Nothing -> Nothing


concatMapMaybeAll :: (a -> Maybe [b]) -> [a] -> Maybe [b]
concatMapMaybeAll f xs = case mapMaybeAll f xs of
                             Just a -> Just $ concat a
                             Nothing -> Nothing


idMaybeAll = mapMaybeAll id
concatIdMaybeAll = concatMapMaybeAll id



-- | pick all subsets (maintaining order) with a length of n
--   n must be greater or equal to the length of the list passed in
selection :: Int -> [a] -> [[a]]
selection n xs = remove (len-n) len [] xs
    where
        len = length xs
        
        remove lrem lxs done todo =
                if lrem == lxs then [reverse done]
                else if null todo then []
                else remove lrem (lxs-1) (t:done) odo ++ remove (lrem-1) (lxs-1) done odo
            where (t:odo) = todo



-- | all permutations of a list
permute :: [a] -> [[a]]
permute [] = [[]]
permute (x:xs) = concat $ map (\a -> zipWith f (inits a) (tails a)) (permute xs)
    where
        f a b = a ++ [x] ++ b



catLefts :: [Either a b] -> [a]
catLefts (Left x:xs) = x : catLefts xs
catLefts (_:xs) = catLefts xs
catLefts [] = []


fromLeft (Left x) = x
