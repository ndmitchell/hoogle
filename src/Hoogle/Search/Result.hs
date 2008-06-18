
module Hoogle.Search.Result where

import Control.Monad
import General.All
import Hoogle.DataBase.All


data Result = Result
    {resultEntry :: Entry
    ,resultModPkg :: Maybe (Module,Package)
    ,resultView :: [EntryView]
    ,resultScore :: [Score]
    }
    deriving Show

data Score = TextScore TextScore
             deriving (Show,Eq,Ord)

-- return the module it is in, and the text to go beside it
renderResult :: Result -> (Maybe [String], TagStr)
renderResult r = (liftM (moduleName . fst) $ resultModPkg r
                 ,renderEntryText (resultView r) (entryText $ resultEntry r))
