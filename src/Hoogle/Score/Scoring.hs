
module Hoogle.Score.Scoring(scoring) where

import Hoogle.Score.Type
import Data.List
import Control.Arrow
import Data.Ord
import Data.Maybe
import Control.Monad
import System.Random


-- | Given a set of scores, where the first is lower than the second, returns details for how to rank scores.
--   This function is in the 'IO' monad since it may require randomness, and it may output status messages while solving,
--   particularly if in Verbose mode.
scoring :: [(Score,Score)] -> IO String
scoring xs = do
    let cost ys = sum [max 0 $ 1 + vals a - vals b | (a,b) <- xs
                      ,let vals = sum . map (fromRange . fromJust . flip lookup ys) . scoreCosts]
    config <- solveConfig cost [(x::TypeCost, toRange [1..10]) | x <- [minBound..maxBound]]
    return $ unlines ["cost " ++ show a ++ " = " ++ show (fromRange b) | (a,b) <- config]


---------------------------------------------------------------------
-- SOLVER

type Cost = Int

-- zipper on the value
data Range a = Range [a] a [a] deriving Show
toRange (x:xs) = Range [] x xs
fromRange (Range _ x _) = x

type Config = [(TypeCost,Range Int)]

bestConfig f = snd . minimumBy (comparing fst) . map (f &&& id)

nextRange (Range a b c) = [Range as a (b:c) | a:as <- [a]] ++ [Range (b:a) c cs | c:cs <- [c]]
nextConfig = perturb $ \(a,b) -> map ((,) a) $ nextRange b

randomRange (Range x y z) = do
    let xs = reverse x ++ y:z
    i <- randomRIO (0,length xs-1)
    let (x2,y2:z2) = splitAt i xs
    return $ Range (reverse x2) y2 z2

randomConfig = mapM $ \(a,b) -> fmap ((,) a) $ randomRange b


-- | Greedy hill climbing to improve a config
improveConfig :: (Config -> Cost) -> Config -> Config
improveConfig f now | f next < f now = improveConfig f next
                    | otherwise = now
    where next = bestConfig f $ nextConfig now


-- | Try and minimize the cost of the config
solveConfig :: (Config -> Cost) -> Config -> IO Config
solveConfig f x = fmap (bestConfig f) $ replicateM 25 $ do
    putChar '.'
    y <- randomConfig x
    let z = improveConfig f y
    print (f y,f z)
    return z


-- | Perturb one value in the list
perturb :: (a -> [a]) -> [a] -> [[a]]
perturb f [] = [[]]
perturb f (x:xs) = map (:xs) (f x) ++ map (x:) (perturb f xs)
