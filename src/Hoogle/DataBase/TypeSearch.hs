
module Hoogle.DataBase.TypeSearch where

import Data.Binary.Defer

data TypeSearch = TypeSearch

createTypeSearch :: a -> TypeSearch
createTypeSearch _ = TypeSearch

instance BinaryDefer TypeSearch
