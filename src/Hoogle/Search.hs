
module Hoogle.Search(Search(..), SearchMode(..), parseSearch) where


import Hoogle.TypeSig
import Hoogle.TextUtil
import Hoogle.Parser
import Char


data Search = Search String SearchMode


data SearchMode = SearchName String
                | SearchType ConType
                | SearchError String


parseSearch :: String -> Search
parseSearch x = Search x $
                if all (not . isSpace) x2
                then SearchName x2
                else case parseConType x2 of
                         Right x -> SearchError x
                         Left x -> SearchType x
    where
        x2 = trim x
