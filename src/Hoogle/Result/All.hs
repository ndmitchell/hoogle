
module Hoogle.Result.All(
	Result(..), resultText,
	Score, resultScore
	) where

import Hoogle.Result.Text

import Hoogle.Item.All
import General.All
import Data.List
import Data.Maybe


data Result = ResultText {itemResult :: Item, textResult :: TextMatch}

resultText :: [String] -> Item -> Result
resultText names i = ResultText i (resultTextMatch names i)



data Score = ScoreText TextScore
             deriving (Eq,Ord)


resultScore :: Result -> Score
resultScore (ResultText i t) = ScoreText $ textScore i t




{-
renderResult :: Result -> TagStr
renderResult (Result item@Item{itemName=name} _ txt atyp) =
    case itemRest item of
        ItemFunc typ -> Tags [showMod, showName, Str " :: ", showType typ]
        ItemModule -> Tags [showKeyword "module",Str " ",showMod, showName]
        ItemData kw (LHSStr con free) -> Tags [showKeyword (show kw),Str " ",Str con,showMod,showName,Str free]
        rest -> Str $ "renderResult, todo: " ++ name ++ " " ++ show rest
    where
        showKeyword s = TagUnderline $ Str s
    
        showMod = Str $ concatMap (++".") $ modName $ itemMod item
    
        showName = case txt of
                Nothing -> Str name
                Just (TextMatch locs _ _) -> Tags $ f 0 name (mergeTextMatchOne locs)
                    where
                        f p s [] = [Str s]
                        f p s (TextMatchOne i n:xs) = Str pre : TagBold (Str mid) : f (i+n) post xs
                            where
                                (pre,rest) = splitAt (i-p) s
                                (mid,post) = splitAt n rest
        
        showType (TypeArgs x xs) = case atyp of
            Nothing -> Str $ x ++ concat (intersperse " -> " xs)
            Just y -> Tags $ Str x : intersperse (Str " -> ") (zipWith f (typeOrder y) (init xs) ++ [Str $ last xs])
                where f n x = TagColor n (Str x)
-}
