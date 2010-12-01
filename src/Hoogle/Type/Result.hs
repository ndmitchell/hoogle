
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


-- return (module it is in, the text to go beside it, verbose scoring info)
renderResult :: Result -> (Maybe Module, TagStr, String)
renderResult r = (fmap fromLink $ entryModule e
                 ,renderEntryText (resultView r) $ entryText e
                 ,show $ resultScore r)
    where e = fromLink $ resultEntry r
