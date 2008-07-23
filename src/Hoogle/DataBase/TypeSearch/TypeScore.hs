
module Hoogle.DataBase.TypeSearch.TypeScore(
    TypeScore,
    emptyTypeScore, mergeTypeScores,
    addCost,
    scoreBinding, scoreUniqueBinding
    ) where

import General.Code
import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.Instances
import Hoogle.TypeSig.All
import qualified Data.Set as Set


data FwdBwd a = Fwd a | Bwd a
                deriving (Eq,Ord,Show)
     
data VarLit = Var String | Lit String
              deriving (Eq,Show)

isLit Lit{} = True; isLit _ = False
isFwd Fwd{} = True; isFwd _ = False


{-
If a and b are restricted to M, it may be that a=b, therefore one restrict
penalty is appropriate. Therefore restrictions are accumulated and only
one case of each is added, until the final binding set is known.

A (Lit _, Lit _) binding may never arise
-}


data TypeScore = TypeScore
    {score :: Int
    ,unbox :: [String], rebox :: [String]
    ,alias :: Set.Set (FwdBwd String)
    
    ,badInstance :: (TypeContext, TypeContext)
    ,bind :: [(VarLit, VarLit)]
    }
    deriving Show

instance Eq TypeScore where
    (==) = (==) `on` score

instance Ord TypeScore where
    compare = compare `on` score


emptyTypeScore :: Binding -> TypeScore
emptyTypeScore bind = TypeScore 0 [] [] Set.empty ([],[]) bs
    where bs = [(Var a, Var b) | (a,b) <- fromBinding bind]


add v t = t{score = score t + v}


-- if adding would bind two literals together, then fail
addCost :: Cost -> TypeScore -> Maybe TypeScore
addCost (CostAlias a)               t = addAlias scoreAliasFwd (Fwd a) t
addCost (CostReverse (CostAlias a)) t = addAlias scoreAliasBwd (Bwd a) t

addCost (CostUnbox a) t = Just $ add scoreUnbox $ t{unbox = a : unbox t}
addCost (CostReverse (CostUnbox a)) t = Just $ add scoreRebox t{rebox = a : rebox t}

-- A |--> b, always restricts to a fresh variable on RHS
addCost (CostRestrict l v) t = Just $ t{bind = (Lit l, Var v) : bind t}

-- b |--> A
addCost (CostReverse (CostRestrict a b)) t
    | or [l /= a | (Var v, Lit l) <- bind t, v == b] = Nothing -- b |--> (A and C)
    | otherwise = Just $ t{bind = (Var b, Lit a) : bind t}


addAlias score x t
    | x `Set.member` alias t = Just t
    | otherwise = Just $ add score $ t{alias = Set.insert x (alias t)}


scoreUniqueBinding :: UniqueBinding -> TypeScore -> TypeScore
scoreUniqueBinding b t = t{bind = map f $ bind t}
    where
        f (x, Var y) = (x, Var $ uniqueBinding b y)
        f x = x


scoreBinding :: Binding -> TypeScore -> Maybe TypeScore
scoreBinding bs t = if badBinding bind2 then Nothing else Just t{bind=bind2}
    where
        bind2 = nub $ filter (isLit . snd) (bind t) ++
                      [(x, Var b) | (a,b) <- fromBinding bs, (x,Var v) <- bind t, v == a]


-- report a bad binding if any variable is bound to two different lits
-- assume the binding list has been nub'd
badBinding :: [(VarLit,VarLit)] -> Bool
badBinding bind = bad varLit || bad litVar
    where
        varLit = [(v,l) | (Var v, Lit l) <- bind]
        litVar = [(v,l) | (Lit l, Var v) <- bind]

        bad = any ((> 1) . length) . groupFst . sortFst



mergeTypeScores :: Instances -> TypeContext -> TypeContext -> [TypeScore] -> Maybe TypeScore
mergeTypeScores is cquery cresult xs 
        | badBinding bs = Nothing
        | otherwise = Just t{score=calcScore t}
    where
        t = TypeScore 0
            (concatMap unbox xs) (concatMap rebox xs)
            (Set.unions $ map alias xs)
            (cquery \\ ctx, ctx \\ cquery)
            bs
        
        ctx = nub $ concat [f c b | (c,v) <- cresult, (Var a, b) <- bs, a == v ]
        f c (Var v) = [(c,v)]
        f c (Lit l) = [(c,l) | not $ hasInstance is c l]

        bs = nub $ concatMap bind xs


calcScore :: TypeScore -> Int
calcScore t =
    scoreUnbox * length (unbox t) +
    scoreRebox * length (rebox t) +
    scoreAliasFwd * Set.size aliasFwd +
    scoreAliasBwd * Set.size aliasBwd +
    scoreInstanceAdd * length (fst $ badInstance t) +
    scoreInstanceDel * length (snd $ badInstance t) +
    scoreDupVarQuery * f (bind t) +
    scoreDupVarResult * f (map swap $ bind t)
    where
        (aliasFwd,aliasBwd) = Set.partition isFwd $ alias t
    
        f xs = sum $ map (subtract 1 . length) $ groupFst $ sortFst [(a,b) | (Var a,b) <- xs]
