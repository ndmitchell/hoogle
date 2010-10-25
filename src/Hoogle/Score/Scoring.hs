
module Hoogle.Score.Scoring(scoring) where

-- import Hoogle.Score.Solve
import Hoogle.Score.Type


-- | A list of scores where one is lower than the other, returns the score result.
--   In the 'IO' monad since it may require randomness, and it may output status messages while solving,
--   particularly if in Verbose mode.
scoring :: [(Score,Score)] -> IO String
scoring _ = error "todo: scoring" {- do
    res <- return $ solve undefined (error "scoring: todo")
    print res
    return "" -}
