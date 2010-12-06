{-# LANGUAGE CPP #-}

module General.Util where

import General.Base
import System.Directory
import qualified Control.Exception as E
import System.IO
import System.Cmd
import System.Exit

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
#endif



-- | Only append strings if neither one is empty
(++?) :: String -> String -> String
a ++? b = if null a || null b then [] else a ++ b


withDirectory dir cmd = E.bracket
    (do x <- getCurrentDirectory; setCurrentDirectory dir; return x)
    setCurrentDirectory
    (const cmd)


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


compareCaseless :: String -> String -> Ordering
compareCaseless x = compare (map toLower x) . map toLower


-- compare strings, but with an ordering that puts 'a' < 'A' < 'b' < 'B'
compareString :: String -> String -> Ordering
compareString (x:xs) (y:ys) = case compareChar x y of
    EQ -> compareString xs ys
    x -> x
compareString [] [] = EQ
compareString xs ys = if null xs then LT else GT


compareChar :: Char -> Char -> Ordering
compareChar x y = case (compare x y, compare (toLower x) (toLower y)) of
    (EQ, _) -> EQ
    (x, EQ) -> if x == GT then LT else GT
    (_, x ) -> x


system_ :: String -> IO ()
system_ x = do
    res <- system x
    when (res /= ExitSuccess) $ error $ "System command failed: " ++ x
