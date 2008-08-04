
module Hoogle.Operations.RankSolve(Cmp(..), rankSolve) where

import General.Code

-- Privilaged imports
import Hoogle.DataBase.TypeSearch.Cost(Cost(..))

data Cmp = [Cost] :< [Cost]
           deriving Show


rankSolve :: [Cmp] -> IO ()
rankSolve xs = error $ show xs
