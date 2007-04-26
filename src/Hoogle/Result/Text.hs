
module Hoogle.Result.Text(TextMatch, resultTextMatch) where

import Hoogle.Item.All
import Data.Char
import Data.List


data TextMatch = TextMatch {
                    textMatch :: [TextMatchOne],
                    textElse :: Int, -- how many other chars are there
                    textCase :: Int -- how many chars have wrong case
                 }
                 deriving Show

data TextMatchOne = TextMatchOne {textBegin :: Int, textLength :: Int}
                    deriving Show


resultTextMatch :: [String] -> Item -> TextMatch
resultTextMatch names item = TextMatch matches (length $ itemName item) (sum cases)
    where (matches,cases) = unzip $ map (`pickMatchOne` item) names




pickMatchOne :: String -> Item -> (TextMatchOne,Int)
pickMatchOne s i = (TextMatchOne begin (length s), bads)
    where
        ls = map toLower s
        ((begin,bads):_) = [(a, cas s b) | (a,b) <- zip [0..] (tails $ itemName i)
                                         , ls `isPrefixOf` map toLower b]
        
        cas a b = length $ filter id $ zipWith (/=) a b



