
module Hoogle.Search.Result where

import General.Code
import Data.TagStr
import Hoogle.Item.All
import Hoogle.Score.All
import Data.Binary.Defer.Index


data Result = Result
    {resultEntry :: Link Entry
    ,resultView :: [EntryView]
    ,resultScore :: Score
    }
    deriving Show


-- return (module it is in, the text to go beside it, verbose scoring info)
renderResult :: Result -> (Maybe [String], TagStr, String)
renderResult r = (if entryType e == EntryModule then Nothing
                  else liftM (moduleName . fromLink) $ entryModule e
                 ,renderEntryText (resultView r) $ entryText e
                 ,show $ resultScore r)
    where e = fromLink $ resultEntry r
