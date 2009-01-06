
module Data.TagStr where

import Data.Char
import Data.List
import Data.Generics.Uniplate


data TagStr = Str String
            | Tags [TagStr]
            | TagBold TagStr
            | TagUnderline TagStr
            | TagHyperlink String TagStr
            | TagColor Int TagStr
              deriving Show


instance Uniplate TagStr where
    uniplate (Tags xs) = (xs, Tags)
    uniplate (TagBold x) = ([x], \[x] -> TagBold x)
    uniplate (TagUnderline x) = ([x], \[x] -> TagUnderline x)
    uniplate (TagHyperlink i x) = ([x], \[x] -> TagHyperlink i x)
    uniplate (TagColor i x) = ([x], \[x] -> TagColor i x)
    uniplate x = ([], const x)


showTagText :: TagStr -> String
showTagText x = concat [y | Str y <- universe x]


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

