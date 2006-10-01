
module Hoogle.Common.Result where

import Hoogle.Common.Item


data Result = Result {textResult :: TextMatch, itemResult :: Item}
              deriving Show


data TextMatch = TextMatch {
                    textLoc :: Location, -- where does the match happen
                    textElse :: Int, -- how many other chars are there
                    textCase :: Int -- how many chars have wrong case
                 }
                 deriving Show

data Location = Prefix
              | Suffix
              | Infix Int
              deriving Show
