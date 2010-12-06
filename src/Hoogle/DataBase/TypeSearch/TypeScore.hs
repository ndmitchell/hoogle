

module Hoogle.DataBase.TypeSearch.TypeScore(
    TypeScore, newTypeScore, costTypeScore, costsTypeScore
    ) where

import General.Base
import Hoogle.Score.All
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.EntryInfo
import Hoogle.DataBase.Instances
import Hoogle.Type.All


data TypeScore = TypeScore
    {costTypeScore :: !Int
    ,badargs :: Int
    ,badorder :: Bool
    ,bind :: Binding
    ,badInstance :: (TypeContext, TypeContext)
    ,badAlias :: ([String], [String])
    }


instance Show TypeScore where
    show t = unwords $
             ['#' : show (costTypeScore t)] ++
             replicate (badargs t) "badarg" ++
             ["badorder" | badorder t] ++
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
    compare = comparing costTypeScore


newTypeScore :: Instances -> EntryInfo -> EntryInfo -> Bool -> Binding -> TypeScore
newTypeScore is query result inorder bs = t{costTypeScore = calcScore t}
    where
        t = TypeScore 0
            (entryInfoArity result - entryInfoArity query)
            (not inorder)
            bs 
            (entryInfoContext query `diff` ctx)
            (entryInfoAlias query `diff` entryInfoAlias result)

        diff a b = (a \\ b, b \\ a)
        ctx = nub $ concat [f c b | (c,v) <- entryInfoContext result, (b, TVar a) <- bindings bs, a == v]
        f c (TVar v) = [(c,v)]
        f c (TLit l) = [(c,l) | not $ hasInstance is c l]


calcScore :: TypeScore -> Int
calcScore t = costBinding (bind t) + sum (map cost $ costsTypeScoreLocal t)


costsTypeScoreLocal :: TypeScore -> [TypeCost]
costsTypeScoreLocal t =
    CostDeadArg *+ badargs t ++
    [CostArgReorder | badorder t] ++
    CostAliasFwd *+ length (fst $ badAlias t) ++
    CostAliasBwd *+ length (snd $ badAlias t) ++
    CostInstanceAdd *+ length (fst $ badInstance t) ++
    CostInstanceDel *+ length (snd $ badInstance t)
    where (*+) = flip replicate


costsTypeScore :: TypeScore -> [TypeCost]
costsTypeScore t = costsBinding (bind t) ++ costsTypeScoreLocal t
