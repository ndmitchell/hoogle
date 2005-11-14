
module Hoogle.Search(Search(..), parseSearch) where


import Hoogle.TypeSig
import Hoogle.TextUtil
import Hoogle.Parser
import Char


data Search = SearchName String
            | SearchType ConType


parseSearch :: String -> Either Search String
parseSearch x = if all (not . isSpace) x2
                then Left $ SearchName x2
                else case parseConType x2 of
                         Right x -> Right x
                         Left x -> Left $ SearchType x
    where
        x2 = trim x
