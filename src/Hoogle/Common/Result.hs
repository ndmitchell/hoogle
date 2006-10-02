
module Hoogle.Common.Result where

import Hoogle.Common.Item
import General.All
import Data.List


data Result = Result {textResult :: Maybe TextMatch, itemResult :: Item}
              deriving Show


data TextMatch = TextMatch {
                    textLoc  :: Int, -- where does the match happen
                    textElse :: Int, -- how many other chars are there
                    textCase :: Int -- how many chars have wrong case
                 }
                 deriving Show


renderResult :: Result -> TagStr
renderResult (Result txt item@(Item modu (Just name) typ _ rest)) =
    case rest of
        ItemFunc -> Tags [showName name, Str " :: ", showType typ]
        _ -> Str $ show item
    where
        showName nam = case txt of
                Nothing -> Str nam
                Just (TextMatch loc others _) -> Tags [Str pre, TagBold $ Str mid, Str post]
                    where
                        (pre,rest) = splitAt loc nam
                        (mid,post) = splitAt (length nam - others) rest
        
        showType (Just (TypeArgs x xs)) = Str $ x ++ concat (intersperse " -> " xs)

