
module Hoogle.Search(Search(..), SearchMode(..), parseSearch) where


import Hoogle.TypeSig
import Hoogle.TextUtil
import Hoogle.Parser
import Data.Char
import Data.List


data Search = Search String SearchMode
              deriving Show


data SearchMode = SearchName String
                | SearchType ConType
                | SearchError String
                deriving Show


parseSearch :: String -> Search
parseSearch x = Search x $
                if not (null x3) && isHaskellName x3
                then SearchName x3
                else case parseConType x2 of
                         Right x -> SearchError x
                         Left x -> SearchType x
    where
        x2 = trim x
        x3 = trimBrackets x2


trimBrackets =
    (\x -> if "(" `isPrefixOf` x then tail x else x) .
    (\x -> if ")" `isSuffixOf` x then init x else x)


isHaskellName :: String -> Bool
isHaskellName (x:xs) | isAlpha x && all (\a -> isAlphaNum a || a `elem` "_'") xs = True
isHaskellName xs = all (`elem` "!#$%&*+./<>=?@/^|-~") xs
