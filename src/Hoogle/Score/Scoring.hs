
module Hoogle.Score.Scoring(scoring) where

import Hoogle.Score.Type
import Control.Arrow


-- | A list of scores where one is lower than the other, returns the score result.
--   In the 'IO' monad since it may require randomness, and it may output status messages while solving,
--   particularly if in Verbose mode.
scoring :: [(Score,Score)] -> IO String
scoring xs = return $ unlines ["cost " ++ show a ++ " = " ++ show b | (a,b) <- res]
    where f val = sum [max 0 $ vals a - vals b | (a,b) <- xs, let vals = sum . map val . scoreCosts]
          res = solve f [(x::TypeCost,[1..10]) | x <- [minBound..maxBound]]

-- | A set of keys range over a given domain. Given a cost function, find a
--   key/domain mapping that minimizes the cost
solve :: ((key -> domain) -> cost) -> [(key,[domain])] -> [(key,domain)]
solve cost initial = map (second head) initial
