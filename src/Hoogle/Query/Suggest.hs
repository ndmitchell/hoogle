
module Hoogle.Query.Suggest(suggestQuery) where

import General.All
import Data.List
import Hoogle.DataBase.All
import Hoogle.Query.Type


suggestQuery :: DataBase -> Query -> Maybe TagStr
suggestQuery db q | "google" `elem` names q = Just $ Tags [TagHyperlink "http://www.google.com/" (Str "Google"), Str " rocks!"]

suggestQuery db q | any f (names q) = Just $ Str "Can't think of anything more interesting to search for?"
    where f x = length x == 6 && "oogle" `isSuffixOf` x

suggestQuery _ _ = Nothing

{-

hoogleSuggest (Search _ (SearchType (c,t))) |
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
        
querySuggest (Search _ (SearchName xs)) | xs == "google" =
        Just $ Tags [Tag "a" (Str "http://www.google.com/"), Str " rocks!"]

querySuggest (Search _ (SearchName (x:xs))) | xs == "oogle" =
        Just $ Str "Can't think of anything more interesting to search for?"

querySuggest _ = Nothing
-}
