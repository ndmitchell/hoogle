
module Hoogle.Hoogle(
    hoogleParse, hoogleParseError, hoogleSearch, hoogleSuggest, hoogleResults, hoogleRange,
    Search, module Hoogle.Result
    ) where

import Hoogle.Search
import Hoogle.Result
import Hoogle.Match
import Hoogle.TypeSig

import List
import Char


hoogleParse :: String -> Search
hoogleParse = parseSearch


hoogleParseError :: Search -> Maybe String
hoogleParseError (Search _ (SearchError x)) = Just x
hoogleParseError _ = Nothing


hoogleSearch :: Search -> TagStr
hoogleSearch (Search _ (SearchType x)) = showTypeTags x [1..]
hoogleSearch (Search _ (SearchName x)) = Tag "b" $ Str x
hoogleSearch (Search x _) = Str x



hoogleSuggest :: Bool -> Search -> Maybe TagStr

hoogleSuggest _ (Search _ (SearchType (c,t))) |
        any dubiousVar (allTVar t) = Just $ Tags
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

hoogleSuggest _ (Search _ (SearchName (x:xs))) | isDigit x =
        Just $ Str "Remember, I have no notion of numbers"

hoogleSuggest _ (Search _ (SearchName x)) | '\"' `elem` x =
        Just $ Str "Remember, I have no notion of quotes"

hoogleSuggest True (Search _ (SearchName xs)) | xs == "google" =
        Just $ Tags [Tag "a" (Str "http://www.google.com/"), Str " rocks!"]

hoogleSuggest True (Search _ (SearchName (x:xs))) | xs == "oogle" =
        Just $ Str "Can't think of anything more interesting to search for?"

hoogleSuggest _ _ = Nothing




hoogleResults :: FilePath -> Search -> IO [Result]
hoogleResults p (Search _ x) = matchOrdered p x

hoogleRange :: FilePath -> Search -> Int -> Int -> IO [Result]
hoogleRange p (Search _ x) = matchRange p x
