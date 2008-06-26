
module Hoogle.Query.Suggest(suggestQuery) where

import Data.TagStr
import Data.List
import Data.Char
import Data.Maybe
import Hoogle.DataBase.All
import Hoogle.Query.Type


suggestQuery :: [DataBase] -> Query -> Maybe TagStr
suggestQuery db q | "google" `elem` map (map toLower) (names q) =
    Just $ Tags [TagHyperlink "http://www.google.com/" (Str "Google"), Str " rocks!"]

suggestQuery db q | any f (names q) = Just $ Str "Can't think of anything more interesting to search for?"
    where f x = length x == 6 && "oogle" `isSuffixOf` x

suggestQuery db q | isJust $ typeSig q =
    case suggestion db (fromJust $ typeSig q) of
        Nothing -> Nothing
        Just (Left s) -> Just $ Str s
        Just (Right t) -> Just $ Str $ "Did you mean: " ++ show t

suggestQuery db q = Nothing
