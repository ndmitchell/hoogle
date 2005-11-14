
module Hoogle.Hoogle(
    hoogleParse, hoogleSearch, hoogleSuggest, hoogleResults, hoogleRange,
    Search, module Hoogle.Result
    ) where

import Hoogle.Search
import Hoogle.Result
import Hoogle.Match
import Hoogle.TypeSig

import List
import Char


hoogleParse :: String -> Either Search String
hoogleParse = parseSearch


hoogleSearch :: Search -> TagStr
hoogleSearch (SearchType x) = showTypeTags x [1..]
hoogleSearch (SearchName x) = Tag "b" $ Str x



hoogleSuggest :: Bool -> Search -> Maybe TagStr

hoogleSuggest _ (SearchType (c,t)) | any dubiousVar (allTVar t) = Just $ Tags
        [Str "Did you mean: ", Tag "a" (Tags $ f $ showConType (c, mapUnbound safeVar t))]
    where
        dubiousVar x = length x > 1
        safeVar x | dubiousVar x = TLit $ '{' : toUpper (head x) : tail x ++ "}"
                  | otherwise = TVar x
        
        f xs = Str a : (if null b then [] else Tag "b" (Str c) : f (safeTail d))
            where
                (a,b) = break (== '{') xs
                (c,d) = break (== '}') (tail b)
                
                safeTail [] = []
                safeTail (x:xs) = xs
        
{-
hoogleSuggest True (SearchName xs@(_:_:_)) = Just $ Tags
        ["Tip: To search for a type, do "
        ,Tag "a" (Str $ ":: " ++ concat (intersperse " " xs))]
-}

hoogleSuggest _ _ = Nothing




hoogleResults :: FilePath -> Search -> IO [Result]
hoogleResults = matchOrdered

hoogleRange :: FilePath -> Search -> Int -> Int -> IO [Result]
hoogleRange = matchRange 
