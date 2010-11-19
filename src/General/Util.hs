{-# LANGUAGE CPP #-}

module General.Util where

import Control.Monad
import Data.Char
import Data.List
import Data.Function(on)
import System.Directory
import Control.Arrow
import qualified Control.Exception as E
import System.IO

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
#endif



-- | Only append strings if neither one is empty
(++?) :: String -> String -> String
a ++? b = if null a || null b then [] else a ++ b


type URL = String

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c


withDirectory dir cmd = E.bracket
    (do x <- getCurrentDirectory; setCurrentDirectory dir; return x)
    setCurrentDirectory
    (const cmd)


fromLeft (Left x) = x
fromRight (Right x) = x


setEq :: Eq a => [a] -> [a] -> Bool
setEq xs ys = all (`elem` ys) xs && all (`elem` xs) ys




sortOn f = sortBy (compare `on` f)
groupOn f = groupBy ((==) `on` f)


sortFst mr = sortOn fst mr
groupFst mr = groupOn fst mr


groupFsts :: Eq k => [(k,v)] -> [(k,[v])]
groupFsts = map (fst . head &&& map snd) . groupFst

sortGroupFsts mr = groupFsts . sortFst $ mr
sortGroupFst mr = groupFst . sortFst $ mr



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



initLast :: [a] -> ([a], a)
initLast [] = error "initLast, empty list []"
initLast [x] = ([], x)
initLast (x:xs) = (x:a, b)
    where (a,b) = initLast xs



lower = map toLower
upper = map toUpper


split :: Eq a => a -> [a] -> [[a]]
split x [] = []
split x xs = if null b then [a] else a : split x (tail b)
    where (a,b) = break (== x) xs


rep from to x = if x == from then to else x


-- | Like splitAt, but also return the number of items that were split.
--   For performance.
splitAtLength :: Int -> [a] -> (Int,[a],[a])
splitAtLength n xs = f n xs
    where
        f i xs | i == 0 = (n,[],xs)
        f i [] = (n-i,[],[])
        f i (x:xs) = (a,x:b,c)
            where (a,b,c) = f (i-1) xs


isRight Right{} = True; isRight _ = False


readFile' x = do
    src <- readFile x
    length src `seq` return src


ltrim = dropWhile isSpace
rtrim = reverse . ltrim . reverse
trim = ltrim . rtrim


rbreak f xs = case break f $ reverse xs of
    (_, []) -> (xs, [])
    (as, b:bs) -> (reverse bs, b:reverse as)


-- FIXME: This could use a lot more bracket calls!
captureOutput :: IO () -> IO (Maybe String)
#if __GLASGOW_HASKELL__ < 612
captureOutput act = return Nothing
#else
captureOutput act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hlint"
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    res <- readFile' f
    removeFile f
    return $ Just res
#endif
