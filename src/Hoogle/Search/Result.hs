
module Hoogle.Search.Result where

import General.Code
import Data.TagStr
import Hoogle.DataBase.All
import Data.Binary.Defer.Index


data Result = Result
    {resultEntry :: Link Entry
    ,resultView :: [EntryView]
    ,resultScore :: [Score]
    }
    deriving Show

-- TypeScore must be less than TextScore
-- so that when name :: type is searched, type takes preference
data Score = TypeScore TypeScore
           | TextScore TextScore
             deriving (Eq,Ord)

instance Show Score where
    showList xs = showString $ "{" ++ unwords (map show xs) ++ "}"
    show (TypeScore x) = show x
    show (TextScore x) = show x


-- return (module it is in, the text to go beside it, verbose scoring info)
renderResult :: Result -> (Maybe [String], TagStr, String)
renderResult r = (if entryType e == EntryModule then Nothing
                  else liftM (moduleName . fromLink) $ entryModule e
                 ,renderEntryText (resultView r) $ entryText e
                 ,show $ resultScore r)
    where e = fromLink $ resultEntry r