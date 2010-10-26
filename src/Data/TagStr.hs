{-# LANGUAGE DeriveDataTypeable #-}

-- | A module representing strings with formatting.
module Data.TagStr(TagStr(..), showTagText, showTagConsole, showTagHTML, showTagHTMLWith) where

import Data.Char
import Data.List
import Data.Data
import Data.Generics.Uniplate
import Data.Maybe
import Numeric


data TagStr = Str String -- ^ Plain text.
            | Tags [TagStr] -- ^ A list of tags one after another.
            | TagBold TagStr -- ^ Bold text.
            | TagUnderline TagStr -- ^ Underlined text.
            | TagHyperlink String TagStr -- ^ A hyperlink to a URL.
            | TagColor Int TagStr -- ^ Colored text. Index into a 0-based palette.
              deriving (Data,Typeable,Ord,Show,Eq)


instance Uniplate TagStr where
    uniplate (Tags xs) = (xs, Tags)
    uniplate (TagBold x) = ([x], \[x] -> TagBold x)
    uniplate (TagUnderline x) = ([x], \[x] -> TagUnderline x)
    uniplate (TagHyperlink i x) = ([x], \[x] -> TagHyperlink i x)
    uniplate (TagColor i x) = ([x], \[x] -> TagColor i x)
    uniplate x = ([], const x)


-- | Show a 'TagStr' as a string, without any formatting.
showTagText :: TagStr -> String
showTagText x = concat [y | Str y <- universe x]


-- | Show a 'TagStr' on a console with ANSI escape sequences.
showTagConsole :: TagStr -> String
showTagConsole x = f [] x
    where
        f a (Str x) = x

        f a t =
            case getCode t of
                Nothing -> g a
                Just val -> tag (val:a) ++ g (val:a) ++ tag a
            where g a = concatMap (f a) (children t)
        
        getCode (TagBold _) = Just "1"
        getCode (TagHyperlink url _) = if null url then Nothing else Just "4"
        getCode (TagUnderline _) = Just "4"
        getCode (TagColor n _) | n <= 5 && n >= 0 = Just ['3', intToDigit (n + 1)]
        getCode _ = Nothing

        tag stack = chr 27 : '[' : (concat $ intersperse ";" $ "0":reverse stack) ++ "m"


-- | Show a 'TagStr' as HTML, using CSS classes for color styling.
showTagHTML :: TagStr -> String
showTagHTML = showTagHTMLWith (const Nothing)


-- | Show TagStr with an override for specific tags.
showTagHTMLWith :: (TagStr -> Maybe String) -> TagStr -> String
showTagHTMLWith f x = g x
    where
        g x | isJust (f x) = fromJust $ f x
        g (Str x) = escapeHTML x
        g (Tags xs) = concatMap g xs
        g (TagBold x) = "<b>" ++ showTagHTML x ++ "</b>"
        g (TagUnderline x) = "<i>" ++ showTagHTML x ++ "</i>"
        -- FIXME: this is overly specific!
        g (TagHyperlink "" x) = g (TagHyperlink url x)
            where str = showTagText x
                  url = if "http:" `isPrefixOf` str then str else "?hoogle=" +% str
        g (TagHyperlink url x) = "<a href=\"" +& url ++ "\">" ++ showTagHTML x ++ "</a>"
        g (TagColor i x) = "<span class='c" ++ show i ++ "'>" ++ showTagHTML x ++ "</span>"


-- FIXME: Should not be here!
a +& b = a ++ escapeHTML b
a +% b = a ++ escapeCGI b
escapeHTML = concatMap f
    where
        f '\"' = "&quot;"
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f '\n' = "<br/>"
        f x = [x]
escapeCGI = concatMap f
    where
        f x | isAlphaNum x || x `elem` "-" = [x]
            | x == ' ' = "+"
            | otherwise = '%' : ['0'|length s == 1] ++ s
            where s = showHex (ord x) ""

