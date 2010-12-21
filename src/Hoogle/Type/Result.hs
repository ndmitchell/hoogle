
module Hoogle.Type.Result where

import Hoogle.Type.TagStr
import Hoogle.Type.Item
import Hoogle.Score.All
import Data.Binary.Defer.Index


data Result = Result
    {resultEntry :: Link Entry
    ,resultView :: [EntryView]
    ,resultScore :: Score
    }
    deriving Show


-- return the entry rendered with respect to the EntryView
renderResult :: Result -> TagStr
renderResult r = renderEntryText (resultView r) $ entryText e
    where e = fromLink $ resultEntry r
