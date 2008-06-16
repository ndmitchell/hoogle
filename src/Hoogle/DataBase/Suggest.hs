
module Hoogle.DataBase.Suggest where

import Data.Binary.Defer

data Suggest = Suggest

createSuggest :: a -> Suggest
createSuggest _ = Suggest

instance BinaryDefer Suggest
