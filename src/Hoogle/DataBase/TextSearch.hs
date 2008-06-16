
module Hoogle.DataBase.TextSearch where

import Data.Binary.Defer

data TextSearch = TextSearch

createTextSearch :: a -> TextSearch
createTextSearch _ = TextSearch

instance BinaryDefer TextSearch
