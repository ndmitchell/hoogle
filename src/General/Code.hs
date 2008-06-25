
module General.Code where

import Data.List
import System.Directory
import qualified Data.IntSet as IntSet


fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

on f g x y = f (g x) (g y)


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
fromRight (Right x) = x


iff :: Bool -> a -> Maybe a
iff b a = if b then Just a else Nothing


setEq :: Eq a => [a] -> [a] -> Bool
setEq xs ys = all (`elem` ys) xs && all (`elem` xs) ys


elemEnum :: Enum a => a -> [a] -> Bool
elemEnum x ys = fromEnum x `elem` map fromEnum ys



data FileType = File | Directory | NotFound
                deriving (Eq,Show)

fileType :: FilePath -> IO FileType
fileType x = do
    b <- doesFileExist x
    if b then return File else do
        b <- doesDirectoryExist x
        return $ if b then Directory else NotFound


sortOn :: Ord k => (v -> k) -> [v] -> [v]
sortOn f = sortBy (compare `on` f)


sortFst :: Ord k => [(k,v)] -> [(k,v)]
sortFst = sortOn fst



nubIntOn :: (v -> Int) -> [v] -> [v]
nubIntOn f = g IntSet.empty
    where
        g m [] = []
        g m (x:xs) | IntSet.member i m = g m xs
                   | otherwise = x : g (IntSet.insert i m) xs
            where i = f x
