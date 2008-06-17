
module Hoogle.DataBase.Suggest where

import Data.Binary.Defer

data Suggest = Suggest deriving Show

createSuggest :: a -> Suggest
createSuggest _ = Suggest

instance BinaryDefer Suggest where
    put _ = return ()
    get = return Suggest
