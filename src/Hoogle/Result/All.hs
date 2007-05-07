
module Hoogle.Result.All(
    Result(..), resultText, renderResult,
    TypeMatch(..), TypeDiff(..), resultType,
    Score, resultScore
    ) where

import Hoogle.Result.TextMatch
import Hoogle.Result.TypeMatch

import Hoogle.Item.All
import General.All
import Data.List
import Data.Maybe


data Result = ResultText {itemResult :: Item, textResult :: TextMatch}
            | ResultType {itemResult :: Item, typeResult :: TypeMatch}
              deriving Show

resultText :: [String] -> Item -> Result
resultText names i = ResultText i (resultTextMatch names i)

resultType :: TypeMatch -> Item -> Result
resultType a b = ResultType b a


data Score = ScoreText TextScore
           | ScoreType TypeScore
             deriving (Eq,Ord)


resultScore :: Result -> Score
resultScore (ResultText i t) = ScoreText $ textScore i t
resultScore (ResultType i t) = ScoreType $ typeScore i t



renderResult :: Result -> TagStr
renderResult (ResultText item@Item{itemName=name} txt) =
    case itemRest item of
        ItemFunc typ -> Tags [showMod, showName, Str " :: ", showType typ]
        ItemModule -> Tags [showKeyword "module",Str " ",showMod, showName]
        ItemData kw (LhsStr con free) -> Tags [showKeyword (show kw),Str " ",Str con,showMod,showName,Str free]
        rest -> Str $ "renderResult, todo: " ++ name ++ " " ++ show rest
    where
        showKeyword s = TagUnderline $ Str s
    
        showMod = Str $ concatMap (++".") $ modName $ itemMod item
    
        showName = renderResultText item txt
        
        showType (TypeStr x xs) = Str $ x ++ concat (intersperse " -> " xs) {- case Nothing of
            Nothing -> Str $ x ++ concat (intersperse " -> " xs)
            Just y -> Tags $ Str x : intersperse (Str " -> ") (zipWith f (typeOrder y) (init xs) ++ [Str $ last xs])
                where f n x = TagColor n (Str x) -}

renderResult (ResultType item@Item{itemName=name} typ) =
    case itemRest item of
        ItemFunc typ -> Tags [showMod, showName, Str " :: ", showType typ]
        ItemModule -> Tags [showKeyword "module",Str " ",showMod, showName]
        ItemData kw (LhsStr con free) -> Tags [showKeyword (show kw),Str " ",Str con,showMod,showName,Str free]
        rest -> Str $ "renderResult, todo: " ++ name ++ " " ++ show rest
    where
        showKeyword s = TagUnderline $ Str s
    
        showMod = Str $ concatMap (++".") $ modName $ itemMod item
    
        showName = Str name
        
        showType (TypeStr x xs) = Str $ x ++ concat (intersperse " -> " xs)
