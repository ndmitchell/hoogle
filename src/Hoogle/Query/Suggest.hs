
module Hoogle.Query.Suggest(suggestQuery) where

import Data.TagStr
import General.Code
import Hoogle.DataBase.All
import Hoogle.Query.Type
import Hoogle.Query.Render


suggestQuery :: [DataBase] -> Query -> Maybe TagStr
suggestQuery db q | "google" `elem` map (map toLower) (names q) =
    Just $ Tags [TagHyperlink "http://www.google.com/" (Str "Google"), Str " rocks!"]

suggestQuery db q | any f (names q) = Just $ Str "Can't think of anything more interesting to search for?"
    where f x = length x == 6 && "oogle" `isSuffixOf` x

suggestQuery db q | isJust $ typeSig q =
    case suggestion db (fromJust $ typeSig q) of
        Nothing -> Nothing
        Just (Left s) -> Just $ TagBold $ Str s
        Just (Right t) -> Just $ Tags [TagBold $ Str "Did you mean: ", TagHyperlink ("query:" ++ q2) $ Str q2]
            where q2 = showTagText $ renderQuery q{typeSig = Just t}

suggestQuery db q = Nothing
