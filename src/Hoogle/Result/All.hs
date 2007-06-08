
module Hoogle.Result.All(
    Result(..), resultText, renderResult,
    TypeMatch(..), TypeDiff(..), resultType,
    Score, resultScore, verboseResult
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
renderResult r =
    case itemRest item of
        ItemFunc typ -> Tags [showMod, showName, Str " :: ", showType typ]
        ItemModule -> Tags [showKeyword "module",Str " ",showMod, showName]
        ItemData kw (LhsStr con free) -> Tags [showKeyword (show kw),Str " ",Str con,showMod,showName,Str free]
        ItemAlias (LhsStr con free) x -> Tags [showKeyword "type",Str " ",showName,Str " = ",showType x]
        rest -> Str $ "renderResult, todo: " ++ name ++ " " ++ show rest
    where
        item@Item{itemName=name} = itemResult r
    
        showKeyword s = TagUnderline $ Str s
    
        showMod = Str $ concatMap (++".") $ modName $ itemMod item
    
        showName = case r of
                       ResultText _ txt -> renderResultText item txt
                       _ -> Str name
        
        showType (TypeStr x xs) = Str $ x ++ concat (intersperse " -> " xs)


verboseResult :: Result -> String
verboseResult (ResultText{}) = ""
verboseResult (ResultType _ t) = verboseTypeMatch t
