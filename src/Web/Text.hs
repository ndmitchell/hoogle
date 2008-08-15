
module Web.Text where

import General.Code
import Data.TagStr
import Numeric


-- | Only append strings if neither one is empty
(+?) :: String -> String -> String
a +? b = if null a || null b then [] else a ++ b

-- | Escape the second argument as HTML before appending
(+&) :: String -> String -> String
a +& b = a ++ escapeHTML b

-- | Escape the second argument as a CGI query string before appending
(+%) :: String -> String -> String
a +% b = a ++ escapeCGI b


escapeHTML = concatMap f
    where
        f '\"' = "&quot;"
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '\n' = "<br/>"
        f x = [x]

escapeCGI = concatMap f
    where
        f x | isAlphaNum x || x `elem` "-" = [x]
            | x == ' ' = "+"
            | otherwise = '%' : ['0'|length s == 1] ++ s
            where s = showHex (ord x) ""


showTagHTML = showTagHTMLWith (const Nothing)


showTagHTMLWith :: (TagStr -> Maybe String) -> TagStr -> String
showTagHTMLWith f x = g x
    where
        g x | isJust (f x) = fromJust $ f x
        g (Str x) = escapeHTML x
        g (Tags xs) = concatMap g xs
        g (TagBold x) = "<b>" ++ showTagHTML x ++ "</b>"
        g (TagUnderline x) = "<i>" ++ showTagHTML x ++ "</i>"
        g (TagHyperlink "" x) = g (TagHyperlink url x)
            where str = showTagText x
                  url = if "http:" `isPrefixOf` str then str else "?q=" +% str
        g (TagHyperlink url x) = "<a href=\"" +& url ++ "\">" ++ showTagHTML x ++ "</a>"
        g (TagColor i x) = "<span class='c" ++ show i ++ "'>" ++ showTagHTML x ++ "</span>"


-- TODO: Should only break on spaces
trimTags :: Int -> TagStr -> TagStr
trimTags n (Tags xs) = Tags $ f n xs
    where
        f n [] = []
        f n (x:xs) | m <  n = x : f (n-m) xs
                   | otherwise = [trimTags n x, Str "..."]
            where m = length (showTagText x)
trimTags n x | length (showTagText x) > n = Tags []
             | otherwise = x


onStr :: (String -> String) -> TagStr -> TagStr
onStr f (Str x) = Str $ f x
onStr f x = x
