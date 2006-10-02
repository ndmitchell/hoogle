
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
        ItemFunc -> Str $ showName name ++ " :: " ++ showType typ
        _ -> Str $ show item
    where
        showName nam = nam
        showType (Just (TypeArgs x xs)) = x ++ concat (intersperse " -> " xs)

