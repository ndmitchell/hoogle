
module Hoogle.Query.Suggest(suggestQuery) where

import General.All
import Data.List
import Data.Char
import Data.Generics.UniplateOn
import Hoogle.DataBase.All
import Hoogle.TypeSig.All
import Hoogle.Query.Type
import Hoogle.Query.Render


suggestQuery :: DataBase -> Query -> Maybe TagStr
suggestQuery db q | "google" `elem` names q = Just $ Tags [TagHyperlink "http://www.google.com/" (Str "Google"), Str " rocks!"]

suggestQuery db q | any f (names q) = Just $ Str "Can't think of anything more interesting to search for?"
    where f x = length x == 6 && "oogle" `isSuffixOf` x

suggestQuery db q | q /= q2 = Just $ suggestSearch q2
    where
        newvar = head $ [c:cs | c <- ['a'..'z'], cs <- "":map show [1..]] \\ vars
        vars = [x | TVar x <- universeQueryType q]
        
        q2 = transformQueryType g $ transformQueryType f q
        f (TLit x) = TApp (TLit x) []
        f (TApp (TApp x []) y) = TApp x y
        f x = x

        g (TApp (TLit x) xs) | length xs `notElem` is && not (null is) =
                tApp (TLit x) $ take i $ xs ++ repeat (TVar $ '\0' : newvar)
            where
                i = maximum is
                is = getKinds db x
        g (TApp x xs) = tApp x xs
        g x = x

suggestQuery db q | q /= q2 = Just $ suggestSearch q2
    where
        q2 = transformQueryType f q

        f (TVar (x:xs)) | not $ null xs = TLit ('\0' : toUpper x : xs)
        f x = x

suggestQuery _ _ = Nothing




suggestSearch :: Query -> TagStr
suggestSearch x = TagHyperlink (showUrl x) (showText x)
    where
        showUrl = showTag . renderQuery . transformQueryType f
        f (TLit ('\0':xs)) = TLit xs
        f x = x

        showText = Tags . zeros . showTag . renderQuery . transformQueryType g
        g (TLit ('\0':xs)) = TLit ('\0':xs ++ "\0")
        g (TVar ('\0':xs)) = TVar ('\0':xs ++ "\0")
        g x = x

        zeros xs | null b = [Str a]
                 | otherwise = Str a : TagBold (Str c) : zeros (drop 1 d)
            where
                (a,b) = break (== '\0') xs
                (c,d) = break (== '\0') (tail b)
