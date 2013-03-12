
module Hoogle.Type.Result where

import Hoogle.Type.TagStr
import Hoogle.Type.Item
import Hoogle.Score.All


data Result = Result
    {resultEntry :: Entry
    ,resultView :: [EntryView]
    ,resultScore :: Score
    }
    deriving (Eq, Show)


-- return the entry rendered with respect to the EntryView
renderResult :: Result -> TagStr
renderResult r = renderEntryText (resultView r) $ entryText $ resultEntry r
