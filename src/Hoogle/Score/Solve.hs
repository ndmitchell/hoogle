
module Hoogle.Score.Solve(solve) where

import Control.Arrow

-- | A set of keys range over a given domain. Given a cost function, find a
--   key/domain mapping that minimizes the cost
solve :: ((key -> domain) -> cost) -> [(key,[domain])] -> [(key,domain)]
solve cost initial = map (second head) initial
