
module General.Type where

import Data.Char
import Data.List


data TagStr = Str String
            | Tags [TagStr]
            | TagBold TagStr
            | TagUnderline TagStr
            | TagHyperlink String TagStr
            | TagColor Int TagStr


tagInner (TagBold x) = x
tagInner (TagUnderline x) = x
tagInner (TagHyperlink _ x) = x
tagInner (TagColor _ x) = x


showTag :: TagStr -> String
showTag x = f x
    where
        f (Str x) = x
        f (Tags x) = concatMap f x
        f x = f $ tagInner x


showTagConsole :: TagStr -> String
showTagConsole x = f [] x
    where
        f a (Str x) = x
        f a (Tags xs) = concatMap (f a) xs

        f a t =
            case getCode t of
                Nothing -> f a x
                Just val -> tag (val:a) ++ f (val:a) x ++ tag a
            where x = tagInner t
        
        getCode (TagBold _) = Just "1"
        getCode (TagHyperlink _ _) = Just "4"
        getCode (TagUnderline _) = Just "4"
        getCode (TagColor n _) | n <= 5 && n >= 0 = Just ['3', intToDigit (n + 1)]
        getCode _ = Nothing
        
        tag stack = chr 27 : '[' : (concat $ intersperse ";" $ ("0":reverse stack)) ++ "m"




instance Show TagStr where
    show (Str x) = x
    show (Tags x) = concatMap show x
    show x = show $ tagInner x



data Response = Warn String
              | Error String


instance Show Response where
    showList = showString . unlines . map show
    show (Warn x) =  "Warning: " ++ x
    show (Error x) = "Error:   " ++ x


isError (Error _) = True; isError _ = False


anyError :: [Response] -> Bool
anyError = any isError
