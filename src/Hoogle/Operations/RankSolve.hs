
module Hoogle.Operations.RankSolve(Cmp(..), rankSolve) where

import General.Code
import qualified Data.IntMap as IntMap

-- Privilaged imports
import Hoogle.DataBase.TypeSearch.Cost(Cost(..))


-- invariant, costs must remain sorted
data Cmp = [Cost] :< [Cost]
           deriving (Show,Eq)


rankSolve :: [Cmp] -> IO ()
rankSolve xs | missing /= [] = error $ "Missing costs: " ++ show missing
             | otherwise = putStr $ showBind res
    where
        res = fix (solve xs2) emptyBind
        xs2 = nub $ concatMap simplify xs
        missing = costs \\ concat [a ++ b | a :< b <- xs2]


costs = [minBound..maxBound] :: [Cost]

type Bind = IntMap.IntMap (Int,Int)

emptyBind :: Bind
emptyBind = IntMap.fromAscList [(fromEnum i, (1,1000)) | i <- costs]


showBind :: Bind -> String
showBind mp = unlines ["cost " ++ padR ncosts (show (toEnum a :: Cost)) ++
                       " = " ++ padL 4 (show b) ++ "  -- " ++ show b ++ ".." ++ show c 
                      | (a,(b,c)) <- IntMap.toList mp]
    where ncosts = maximum $ map (length . show) costs

padL, padR :: Int -> String -> String
padL n xs = replicate (n - length xs) ' ' ++ xs
padR n xs = xs ++ replicate (n - length xs) ' '



simplify :: Cmp -> [Cmp]
simplify c@(xs :< ys)
        | null xs2 = []
        | null ys2 = error $ "rankSolve, contradiction exists: " ++ show c
        | otherwise = [xs2 :< ys2]
    where
        common = intersect xs ys
        xs2 = xs \\ common
        ys2 = ys \\ common


solve :: [Cmp] -> Bind -> Bind
solve xs mp = foldl' f mp xs
    where
        -- all x in xs, x.max = ys.max - 1
        -- all y in ys, y.min = xs.min + 1
        f mp (xs :< ys) = upd (id *** min (maxRhs - 1)) xs $
                          upd (max (minLhs + 1) *** id) ys mp
            where
                minLhs = grab fst xs mp
                maxRhs = grab snd ys mp


        grab side xs mp = sum $ map (side . (mp IntMap.!) . fromEnum) xs

        -- does not deal well with multiple elements on either side
        upd op [x] mp = IntMap.update (Just . op) (fromEnum x) mp
        upd op _ mp = mp
