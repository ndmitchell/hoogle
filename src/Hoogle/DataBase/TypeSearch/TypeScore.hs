

module Hoogle.DataBase.TypeSearch.TypeScore(
    TypeScore, newTypeScore, costTypeScore
    ) where

import General.Code
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.EntryInfo
import Hoogle.DataBase.Instances
import Hoogle.TypeSig.All
import qualified Data.Set as Set


data TypeScore = TypeScore
    {score :: !Int
    ,badargs :: Int
    ,bind :: Binding
    ,badInstance :: (TypeContext, TypeContext)
    ,badAlias :: ([String], [String])
    } deriving Show


costTypeScore :: TypeScore -> Int
costTypeScore = score

{-
instance Show TypeScore where
    show t = concat $ intersperse "," $
             [show $ score t] ++
             replicate (badargs t) "badarg" ++
             map ("unbox "++) (unbox t) ++
             map ("rebox "++) (rebox t) ++
             map f (Set.toList $ alias t) ++
             map (g "+") (fst $ badInstance t) ++
             map (g "-") (snd $ badInstance t) ++
             map ( "alias "++) (fst $ badAlias t) ++
             map ("~alias "++) (snd $ badAlias t) ++
             map h (bind t)
        where
            f (Fwd a) = "alias " ++ a
            f (Bwd a) = "~alias " ++ a

            g op (a,b) = op ++ a ++ " " ++ b

            h (a,b) = i a ++ "=" ++ i b
            i (Var a) = a
            i (Lit a) = a
-}

instance Eq TypeScore where
    (==) = (==) `on` score

instance Ord TypeScore where
    compare = compare `on` score


newTypeScore :: Instances -> EntryInfo -> EntryInfo -> Binding -> TypeScore
newTypeScore is result query bs = t{score = calcScore t}
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
calcScore t =
    scoreDeadArg * badargs t +
    costBinding (bind t) +
    scoreAliasFwd * length (fst $ badAlias t) +
    scoreAliasBwd * length (snd $ badAlias t) +
    scoreInstanceAdd * length (fst $ badInstance t) +
    scoreInstanceDel * length (snd $ badInstance t)
