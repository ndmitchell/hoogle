
module Hoogle.DataBase.Instances(
    Instances, createInstances, normContext, followInstances
    ) where

import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Data.Binary.Defer

data Instances = Instances
                 deriving Show

instance BinaryDefer Instances where
    put _ = put0
    get = get0 Instances


createInstances :: [TextItem] -> Instances
createInstances _ = Instances


-- deal with MPTC in this step
-- PostCondition: All classes must be "TApp (TLit x) [TVar y]"
normContext :: Instances -> TypeSig -> TypeSig
normContext _ (TypeSig a b) = TypeSig [] b


followInstances :: Instances -> TypeSig -> [TypeSig]
followInstances _ _ = []
