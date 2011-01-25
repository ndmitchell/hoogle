
module Hoogle.Query.Suggest(suggestQuery) where

import General.Base
import General.Util
import Hoogle.DataBase.All
import Hoogle.Query.Type
import Hoogle.Query.Render
import Hoogle.Type.All


suggestQuery :: [DataBase] -> Query -> Maybe TagStr

-- They searched for Google (pay homage)
suggestQuery db q | "google" `elem` map (map toLower) (names q) =
    Just $ Tags [TagLink "http://www.google.com/" (Str "Google"), Str " rocks!"]

-- They searched for ?oogle (mock)
suggestQuery db q | any f (names q) = Just $ Str "Can't think of anything more interesting to search for?"
    where f x = length x == 6 && "oogle" `isSuffixOf` x

-- They searched for "Int to Float", they meant "Int -> Float"
suggestQuery db q@Query{typeSig=Nothing, names=names}
    | length parts > 1 && all (not . null) parts = Just $ didYouMean q2
    where parts = split "to" names
          q2 = fixup db $ q{names = [] ,typeSig = Just $ TypeSig [] t2}
          t2 = TFun $ map (toApp . map toLitVar) parts

-- They search for "Maybe a", did they mean ":: Maybe a"
suggestQuery db q@Query{typeSig=Nothing, names=names} | length names > 1 && all f names = Just $ didYouMean q2
    where q2 = fixup db $ q{names = [], typeSig = Just $ TypeSig [] $ toApp $ map toLitVar names}
          f (x:xs) = if null xs then isLower x else isUpper x

-- See what the type signature suggests from the database
suggestQuery db q@Query{typeSig=Just t} =
    case suggestion db t of
        Nothing -> Nothing
        Just (Left s) -> Just $ TagBold $ Str s
        Just (Right t) -> Just $ didYouMean $ q{typeSig = Just t}

suggestQuery db q = Nothing



didYouMean :: Query -> TagStr
didYouMean q = Tags [TagBold $ Str "Did you mean: ", TagLink "" $ Str s]
    where s = showTagText $ renderQuery q


fixup :: [DataBase] -> Query -> Query
fixup db q@Query{typeSig=Just t} =
    case suggestion db t of
        Just (Right t) -> q{typeSig=Just t}
        _ -> q
fixup db q = q


toLitVar xs@(x:_) = if isLower x then TVar xs else TLit xs
toApp (x:xs) = TApp x xs
