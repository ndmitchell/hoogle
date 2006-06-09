
module Hoogle.Search(Search(..), SearchMode(..), parseSearch) where


import Hoogle.TypeSig
import Hoogle.TextUtil
import Hoogle.Parser
import Data.Char


data Search = Search String SearchMode
              deriving Show


data SearchMode = SearchName String
                | SearchType ConType
                | SearchError String
                deriving Show


parseSearch :: String -> Search
parseSearch x = Search x $
                if isHaskellName x2
                then SearchName x2
                else case parseConType x2 of
                         Right x -> SearchError x
                         Left x -> SearchType x
    where
        x2 = trim x


isHaskellName :: String -> Bool
isHaskellName (x:xs) | isAlpha x && all (\a -> isAlphaNum a || a `elem` "_'") xs = True
isHaskellName xs = all (`elem` "!#$%&*+./<>=?@/^|-~") xs
isHaskellName _ = False
