
module Hoogle.DataBase.TypeSearch where

import Data.Binary.Defer

data TypeSearch = TypeSearch deriving Show

createTypeSearch :: a -> TypeSearch
createTypeSearch _ = TypeSearch

instance BinaryDefer TypeSearch where
    put _ = return ()
    get = return TypeSearch
