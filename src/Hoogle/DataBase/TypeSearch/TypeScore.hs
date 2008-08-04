

module Hoogle.DataBase.TypeSearch.TypeScore(
    TypeScore, newTypeScore, costTypeScore, costsTypeScore
    ) where

import General.Code
import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.EntryInfo
import Hoogle.DataBase.Instances
import Hoogle.TypeSig.All
import qualified Data.Set as Set


data TypeScore = TypeScore
    {costTypeScore :: !Int
    ,badargs :: Int
    ,bind :: Binding
    ,badInstance :: (TypeContext, TypeContext)
    ,badAlias :: ([String], [String])
    }


instance Show TypeScore where
    show t = unwords $
             ['#' : show (costTypeScore t)] ++
             replicate (badargs t) "badarg" ++
             [show $ bind t] ++
             both inst (badInstance t) ++
             both alis (badAlias t)
        where
            both f (a,b) = map (f "+") a ++ map (f "-") b
            inst op (c,v) = c ++ op ++ v
            alis op c = op ++ c


instance Eq TypeScore where
    (==) = (==) `on` costTypeScore

instance Ord TypeScore where
    compare = compare `on` costTypeScore


newTypeScore :: Instances -> EntryInfo -> EntryInfo -> Binding -> TypeScore
newTypeScore is query result bs = t{costTypeScore = calcScore t}
    where
        t = TypeScore 0
            (entryInfoArity result - entryInfoArity query)
            bs 
            (entryInfoContext query `diff` ctx)
            (entryInfoAlias query `diff` entryInfoAlias result)

        diff a b = (a \\ b, b \\ a)
        ctx = nub $ concat [f c b | (c,v) <- entryInfoContext result, (TVar a, b) <- bindings bs, a == v]
        f c (TVar v) = [(c,v)]
        f c (TLit l) = [(c,l) | not $ hasInstance is c l]


calcScore :: TypeScore -> Int
calcScore t = costBinding (bind t) + score (costsTypeScoreLocal t)


costsTypeScoreLocal :: TypeScore -> [Cost]
costsTypeScoreLocal t =
    CostDeadArg *+ badargs t ++
    CostAliasFwd *+ length (fst $ badAlias t) ++
    CostAliasBwd *+ length (snd $ badAlias t) ++
    CostInstanceAdd *+ length (fst $ badInstance t) ++
    CostInstanceDel *+ length (snd $ badInstance t)


costsTypeScore :: TypeScore -> [Cost]
costsTypeScore t = costsBinding (bind t) ++ costsTypeScoreLocal t
