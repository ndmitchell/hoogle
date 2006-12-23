
module Hoogle.Common.Result where

import Hoogle.Common.Item
import General.All
import Data.List
import Data.Maybe


data Result a = Result {textResult :: Maybe TextMatch, typeResult :: Maybe TypeMatch, itemResult :: Item a}
                deriving Show


data TextMatch = TextMatch {
                    textMatch :: [TextMatchOne],
                    textElse :: Int, -- how many other chars are there
                    textCase :: Int -- how many chars have wrong case
                 }
                 deriving Show

data TextMatchOne = TextMatchOne {textBegin :: Int, textLength :: Int}
                    deriving Show

data TypeMatch = TypeMatch {typeDiff :: [TypeDiff], typeOrder :: [Int]}
                 deriving Show


data TypeDiff = UnwrapLeft | UnwrapRight
              | MultiLeft  | MultiRight
              | ArgMissing
                deriving Show


computeTextMatch :: String -> [(String,TextMatchOne)] -> TextMatch
computeTextMatch name matches = TextMatch (map snd matches) (length name - covered) cases
    where
        covered = sum $ map textLength $ mergeTextMatchOne $ map snd matches
        
        cases = sum $ map f matches
        
        f (query,TextMatchOne b l) = length $ filter id $ zipWith (/=) query name2
            where name2 = take l $ drop b name


mergeTextMatchOne :: [TextMatchOne] -> [TextMatchOne]
mergeTextMatchOne = merge . sortBy cmp
    where
        cmp a b = textBegin a `compare` textBegin b
        
        merge (TextMatchOne b1 l1 : TextMatchOne b2 l2 : xs)
            | b1+l1 >= b2 = merge (TextMatchOne b1 (b2+l2-b1) : xs)
        merge (x:xs) = x : merge xs
        merge [] = []


renderResult :: Result a -> TagStr
renderResult (Result txt atyp item@(Item modu (Just name) typ _ _ rest)) =
    case rest of
        ItemFunc -> Tags [showMod, showName, Str " :: ", showType $ fromJust typ]
        ItemModule -> Tags [showKeyword "module",Str " ",showMod, showName]
        ItemData kw (LHSStr con free) -> Tags [showKeyword (show kw),Str " ",Str con,showMod,showName,Str free]
        _ -> Str $ "renderResult, todo: " ++ name ++ " " ++ show rest
    where
        showKeyword s = TagUnderline $ Str s
    
        showMod = case modu of
                      Nothing -> Str ""
                      Just (Module [] _) -> Tags []
                      Just (Module xs _) -> Tags [Str $ concat $ intersperse "." xs, Str "."]
    
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

