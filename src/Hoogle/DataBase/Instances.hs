
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
--    C (M a) |-> C a
-- Do not load Instances ever
normContext :: Instances -> TypeSig -> TypeSig
normContext _ (TypeSig a b) = TypeSig con b
    where con = [TApp (TLit c) [TVar v] | TApp (TLit c) xs <- a, x <- xs, TVar v <- universe x]


followInstances :: Instances -> TypeSig -> [TypeSig]
followInstances _ _ = []
