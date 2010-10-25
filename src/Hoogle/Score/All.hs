
module Hoogle.Score.All(
    module Hoogle.Score.Scoring,
    Score, TypeCost(..), TextMatch(..), textScore, typeScore
    ) where

import Hoogle.Score.Scoring
import Hoogle.Score.Type hiding (typeCosts)
