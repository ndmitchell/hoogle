
module Hoogle.Query.Suggest(suggestQuery) where

import Data.TagStr
import General.Code
import Hoogle.DataBase.All
import Hoogle.Query.Type
import Hoogle.Query.Render
import Hoogle.TypeSig.All


suggestQuery :: [DataBase] -> Query -> Maybe TagStr

-- They searched for Google (pay homage)
suggestQuery db q | "google" `elem` map (map toLower) (names q) =
    Just $ Tags [TagHyperlink "http://www.google.com/" (Str "Google"), Str " rocks!"]

-- They searched for ?oogle (mock)
suggestQuery db q | any f (names q) = Just $ Str "Can't think of anything more interesting to search for?"
    where f x = length x == 6 && "oogle" `isSuffixOf` x

-- They search for "Maybe a", did they mean ":: Maybe a"
suggestQuery db q@Query{typeSig=Nothing, names=n:ames} | all f (n:ames) = Just $ didYouMean q2
    where q2 = q{names = [], typeSig = Just $ TypeSig [] $ TApp (g n) (map g ames)}
          f (x:xs) = if null xs then isLower x else isUpper x
          g xs@(x:_) = if isLower x then TVar xs else TLit xs


-- See what the type signature suggests from the database
suggestQuery db q | isJust $ typeSig q =
    case suggestion db (fromJust $ typeSig q) of
        Nothing -> Nothing
        Just (Left s) -> Just $ TagBold $ Str s
        Just (Right t) -> Just $ didYouMean $ q{typeSig = Just t}

suggestQuery db q = Nothing



didYouMean :: Query -> TagStr
didYouMean q = Tags [TagBold $ Str "Did you mean: ", TagHyperlink ("query:" ++ s) $ Str s]
    where s = showTagText $ renderQuery q
