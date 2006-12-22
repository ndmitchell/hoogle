
module Hoogle.Common.Result where

import Hoogle.Common.Item
import General.All
import Data.List
import Data.Maybe


data Result = Result {textResult :: Maybe TextMatch, typeResult :: Maybe TypeMatch, itemResult :: Item}
              deriving Show


data TextMatch = TextMatch {
                    textLoc  :: Int, -- where does the match happen
                    textElse :: Int, -- how many other chars are there
                    textCase :: Int -- how many chars have wrong case
                 }
                 deriving Show

data TypeMatch = TypeMatch {typeDiff :: [TypeDiff], typeOrder :: [Int]}
                 deriving Show


data TypeDiff = UnwrapLeft | UnwrapRight
              | MultiLeft  | MultiRight
              | ArgMissing
                deriving Show



renderResult :: Result -> TagStr
renderResult (Result txt atyp item@(Item modu (Just name) typ _ rest)) =
    case rest of
        ItemFunc -> Tags [showMod, showName, Str " :: ", showType $ fromJust typ]
        ItemModule -> Tags [showKeyword "module",Str " ",showMod, showName]
        ItemData kw (LHSStr con free) -> Tags [showKeyword (show kw),Str " ",Str con,showMod,showName,Str free]
        _ -> Str $ show item
    where
        showKeyword s = TagUnderline $ Str s
    
        showMod = case modu of
                      Nothing -> Str ""
                      Just (Module []) -> Tags []
                      Just (Module xs) -> Tags [Str $ concat $ intersperse "." xs, Str "."]
    
        showName = case txt of
                Nothing -> Str name
                Just (TextMatch loc others _) -> Tags [Str pre, TagBold $ Str mid, Str post]
                    where
                        (pre,rest) = splitAt loc name
                        (mid,post) = splitAt (length name - others) rest
        
        showType (TypeArgs x xs) = case atyp of
            Nothing -> Str $ x ++ concat (intersperse " -> " xs)
            Just y -> Tags $ Str x : intersperse (Str " -> ") (zipWith f (typeOrder y) (init xs) ++ [Str $ last xs])
                where f n x = TagColor n (Str x)

