
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


-- PostCondition: All classes must be "TApp (TLit x) [TVar y]"
-- Convert:
--    MPTC a b |-> MPTC1 a, MPTC2 b
--    C (M a) |-> C a (if supported by an instance decl)
-- Do not load Instances unless necessary (probably normally not)
normContext :: Instances -> TypeSig -> TypeSig
normContext _ (TypeSig a b) = TypeSig [] b


followInstances :: Instances -> TypeSig -> [TypeSig]
followInstances _ _ = []
