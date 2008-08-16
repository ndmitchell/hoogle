
module General.Util where

import Control.Monad
import Data.Char
import Data.List
import Debug.Trace
import System.Directory
import System.Exit
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Control.Arrow
import System.IO.Unsafe

infixl 0 `on`

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

on f g x y = f (g x) (g y)

swap (a,b) = (b,a)


fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x2 then x else fix f x2
    where x2 = f x



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



sortOn f = sortBy (compare `on` f)
groupOn f = groupBy ((==) `on` f)


sortFst mr = sortOn fst mr
groupFst mr = groupOn fst mr


groupFsts :: Eq k => [(k,v)] -> [(k,[v])]
groupFsts = map (fst . head &&& map snd) . groupFst

sortGroupFsts mr = groupFsts . sortFst $ mr
sortGroupFst mr = groupFst . sortFst $ mr



nubIntOn :: (v -> Int) -> [v] -> [v]
nubIntOn f = g IntSet.empty
    where
        g m [] = []
        g m (x:xs) | IntSet.member i m = g m xs
                   | otherwise = x : g (IntSet.insert i m) xs
            where i = f x


fold :: a -> (a -> a -> a) -> [a] -> a
fold x f [] = x
fold x f xs = fold1 f xs


fold1 :: (a -> a -> a) -> [a] -> a
fold1 f [x] = x
fold1 f xs = f (fold1 f a) (fold1 f b)
    where (a,b) = halves xs


halves :: [a] -> ([a],[a])
halves [] = ([], [])
halves (x:xs) = (x:b,a)
    where (a,b) = halves xs


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f xs [] = xs
mergeBy f [] ys = ys
mergeBy f (x:xs) (y:ys)
    | f x y /= GT = x : mergeBy f xs (y:ys)
    | otherwise = y : mergeBy f (x:xs) ys


merges :: Ord a => [[a]] -> [a]
merges = fold [] merge

mergesBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesBy f = fold [] (mergeBy f)


concatMapM f = liftM concat . mapM f


unzipEithers :: [Either a b] -> ([a],[b])
unzipEithers [] = ([],[])
unzipEithers (Left x:xs) = (x:a,b)
    where (a,b) = unzipEithers xs
unzipEithers (Right x:xs) = (a,x:b)
    where (a,b) = unzipEithers xs


dropEnd :: (a -> Bool) -> [a] -> [a]
dropEnd f = reverse . dropWhile f . reverse

dropEnds :: (a -> Bool) -> [a] -> [a]
dropEnds f = dropWhile f . dropEnd f


initLast :: [a] -> ([a], a)
initLast [] = error "initLast, empty list []"
initLast [x] = ([], x)
initLast (x:xs) = (x:a, b)
    where (a,b) = initLast xs


(!!+) :: [a] -> (Int,a) -> [a]
(!!+) (x:xs) (0,a) = a:xs
(!!+) (x:xs) (n,a) = x : (!!+) xs (n-1,a)


disjoint :: Eq a => [a] -> Bool
disjoint xs = xs == nub xs


-- useful command line auxiliary
exitMessage :: [String] -> IO a
exitMessage msg = putStr (unlines msg) >> exitFailure


lower = map toLower
upper = map toUpper


split :: Eq a => a -> [a] -> [[a]]
split x [] = []
split x xs = if null b then [a] else a : split x (tail b)
    where (a,b) = break (== x) xs


traceM :: Monad m => String -> m ()
traceM msg = trace msg $ return ()


traceShow :: Show s => s -> a -> a
traceShow x = trace (show x)


fromListMany :: Ord k => [(k,v)] -> Map.Map k [v]
fromListMany = Map.fromAscList . groupFsts . sortFst


consNub :: Eq a => a -> [a] -> [a]
consNub x xs = [x | x `notElem` xs] ++ xs


showLines :: Show a => [a] -> String
showLines = unlines . map show


traceInline :: String -> a -> a
traceInline msg x = unsafePerformIO $ do
    putStr msg
    return x


errorLines :: [String] -> b
errorLines [] = error "errorLines, finished"
errorLines (x:xs) = trace x $ errorLines xs


rep from to x = if x == from then to else x


-- | Like splitAt, but also return the number of items that were split.
--   For performance.
-- TODO: compare to 0 in the inner loop
splitAtLength :: Int -> [a] -> (Int,[a],[a])
splitAtLength n xs = f n xs
    where
        f i xs | i == 0 = (n,[],xs)
        f i [] = (n-i,[],[])
        f i (x:xs) = (a,x:b,c)
            where (a,b,c) = f (i-1) xs

