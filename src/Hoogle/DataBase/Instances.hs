
module Hoogle.DataBase.Instances where

import Hoogle.TextBase.All
import Data.Binary.Defer

data Instances = Instances
                 deriving Show

instance BinaryDefer Instances where
    get = undefined
    put = undefined


createInstances :: [TextItem] -> Instances
createInstances _ = Instances
