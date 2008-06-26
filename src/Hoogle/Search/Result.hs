
module Hoogle.Search.Result where

import General.Code
import Data.TagStr
import Hoogle.DataBase.All


data Result = Result
    {resultEntry :: Entry
    ,resultModPkg :: Maybe (Module,Package)
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
renderResult r = (if entryType (resultEntry r) == EntryModule then Nothing
                  else liftM (moduleName . fst) $ resultModPkg r
                 ,renderEntryText (resultView r) (entryText $ resultEntry r)
                 ,show $ resultScore r)
