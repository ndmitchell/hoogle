{-# LANGUAGE DeriveDataTypeable #-}

-- | A module representing strings with formatting.
module Data.TagStr where

import Data.Char
import Data.List
import Data.Data
import Data.Generics.Uniplate


data TagStr = Str String -- ^ Plain text.
            | Tags [TagStr] -- ^ A list of tags one after another.
            | TagBold TagStr -- ^ Bold text.
            | TagUnderline TagStr -- ^ Underlined text.
            | TagHyperlink String TagStr -- ^ A hyperlink to a URL.
            | TagColor Int TagStr -- ^ Colored text.
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

