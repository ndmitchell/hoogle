{-# LANGUAGE DeriveDataTypeable #-}

-- | A module representing strings with formatting.
module Data.TagStr(TagStr(..), formatTags, showTagText, showTagConsole, showTagHTML, showTagHTMLWith) where

import Data.Char
import Data.List
import Data.Data
import Data.Generics.Uniplate
import Data.Binary.Defer
import Data.Maybe
import Data.Function
import General.Web


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


instance BinaryDefer TagStr where
    put (Str x)            = putByte 0 >> put1 x
    put (Tags x)           = putByte 1 >> put1 x
    put (TagBold x)        = putByte 2 >> put1 x
    put (TagUnderline x)   = putByte 3 >> put1 x
    put (TagHyperlink x y) = putByte 4 >> put2 x y
    put (TagColor x y)     = putByte 5 >> put2 x y

    get = do i <- getByte
             case i of
                0 -> get1 Str
                1 -> get1 Tags
                2 -> get1 TagBold
                3 -> get1 TagUnderline
                4 -> get2 TagHyperlink
                5 -> get2 TagColor


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

        tag stack = chr 27 : '[' : intercalate ";" ("0":reverse stack) ++ "m"


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
                  url = if "http:" `isPrefixOf` str then str else "?hoogle=" ++% str
        g (TagHyperlink url x) = "<a href=\"" ++& url ++ "\">" ++ showTagHTML x ++ "</a>"
        g (TagColor i x) = "<span class='c" ++ show i ++ "'>" ++ showTagHTML x ++ "</span>"


-- each position is a 0-based start and end index
-- currently not allowed to overlap
formatTags :: String -> [((Int,Int),TagStr -> TagStr)] -> TagStr
formatTags o y = tags $ f o 0 $ sortBy (compare `on` fst . fst) y
    where
        f x i [] = str x
        f x i (((from,to),op):ss)
            | i > from = error $ "Data.TagStr.formatTags, not allowed overlapping formats on: " ++ o
            | otherwise = str a ++ [op $ Str c] ++ f d to ss
                where (a,b) = splitAt (from-i) x
                      (c,d) = splitAt (to-from) b

        tags [] = Str ""
        tags [x] = x
        tags xs = Tags xs
        str x = [Str x | x /= ""]
