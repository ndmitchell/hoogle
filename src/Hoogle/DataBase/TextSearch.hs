
module Hoogle.DataBase.TextSearch where

import Data.Binary.Defer

data TextSearch = TextSearch deriving Show

createTextSearch :: a -> TextSearch
createTextSearch _ = TextSearch

instance BinaryDefer TextSearch
