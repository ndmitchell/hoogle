
module Hoogle.Result.Text(
    TextMatch, resultTextMatch, renderResultText,
    TextScore, textScore
    ) where

import Hoogle.Item.All
import General.All
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
    where (matches,cases) = unzip $ concatMap (`pickMatchOne` item) names




pickMatchOne :: String -> Item -> [(TextMatchOne,Int)]
pickMatchOne s i =
        case res of
            (begin,bads):_ -> [(TextMatchOne begin (length s), bads)]
            _ -> []
    where
        ls = map toLower s
        res = [(a, cas s b) | (a,b) <- zip [0..] (tails $ itemName i)
                            , ls `isPrefixOf` map toLower b]
        
        cas a b = length $ filter id $ zipWith (/=) a b



data TextScore = TextScore Int Int Int Int String Int [String]
                 deriving (Eq,Ord)

textScore :: Item -> TextMatch -> TextScore
textScore item txt = TextScore
    (negate $ length $ textMatch txt)
    (textElse txt)
    (textCase txt)
    (itemPriority $ itemRest item)
    (itemName item)
    (length $ modName $ itemMod item)
    (modName $ itemMod item)


renderResultText :: Item -> TextMatch -> TagStr
renderResultText item match = Tags $ zipWith f [0..] (itemName item)
    where
        f i c = (if i `elem` bold then TagBold else id) $ Str [c]
        bold = concat [take len [beg..] | TextMatchOne beg len <- textMatch match]
