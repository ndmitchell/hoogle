{-|
    RULES:

    May only unbox OR rebox once per argument.
-}

module Hoogle.DataBase.TypeSearch.TypeScore(
    TypeScore, typeScoreKey,
    emptyTypeScore, mergeTypeScores,
    addCost,
    scoreBinding, scoreUniqueBinding
    ) where

import General.Code
import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.EntryInfo
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
    ,restrict :: Set.Set (FwdBwd String) -- only necessary before mergeTypeScores
    
    ,badInstance :: (TypeContext, TypeContext)
    ,bind :: [(VarLit, VarLit)]
    }


typeScoreKey = score

instance Show TypeScore where
    show t = concat $ intersperse "," $
             [show $ score t] ++
             map ("unbox "++) (unbox t) ++
             map ("rebox "++) (rebox t) ++
             map f (Set.toList $ alias t) ++
             map (g "+") (fst $ badInstance t) ++
             map (g "-") (snd $ badInstance t) ++
             map h (bind t)
        where
            f (Fwd a) = "alias " ++ a
            f (Bwd a) = "~alias " ++ a

            g op (a,b) = op ++ a ++ " " ++ b

            h (a,b) = i a ++ "=" ++ i b
            i (Var a) = a
            i (Lit a) = a


instance Eq TypeScore where
    (==) = (==) `on` score

instance Ord TypeScore where
    compare = compare `on` score


emptyTypeScore :: Binding -> TypeScore
emptyTypeScore bind = TypeScore 0 [] [] Set.empty Set.empty ([],[]) bs
    where bs = [(Var a, Var b) | (a,b) <- fromBinding bind]


add v t = t{score = score t + v}


-- if adding would bind two literals together, then fail
addCost :: Cost -> TypeScore -> Maybe TypeScore
addCost (CostAlias a)               t = addAlias scoreAliasFwd (Fwd a) t
addCost (CostReverse (CostAlias a)) t = addAlias scoreAliasBwd (Bwd a) t

addCost (CostUnbox a) t = addUnbox t $ add scoreUnbox $ t{unbox = a : unbox t}
addCost (CostReverse (CostUnbox a)) t = addUnbox t $ add scoreRebox t{rebox = a : rebox t}

-- A |--> b, always restricts to a fresh variable on RHS
addCost (CostRestrict l v) t = addRestrict scoreRestrict (Fwd l) $ t{bind = (Lit l, Var v) : bind t}

-- b |--> A
addCost (CostReverse (CostRestrict a b)) t
    | or [l /= a | (Var v, Lit l) <- bind t, v == b] = Nothing -- b |--> (A and C)
    | otherwise = addRestrict scoreUnrestrict (Bwd a) $ t{bind = (Var b, Lit a) : bind t}


addUnbox t r
    | null (unbox t) && null (rebox t) = Just r
    | otherwise = Nothing


addAlias score x t
    | x `Set.member` alias t = Just t
    | otherwise = Just $ add score $ t{alias = Set.insert x $ alias t}

addRestrict score x t
    | x `Set.member` restrict t = Just t
    | otherwise = Just $ add score $ t{restrict = Set.insert x $ restrict t}


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



mergeTypeScores :: Instances -> EntryInfo -> EntryInfo -> [TypeScore] -> Maybe TypeScore
mergeTypeScores is result query xs 
        | badBinding bs = Nothing
        | otherwise = Just t{score = calcScore t + (badargs * scoreDeadArg)}
    where
        t = TypeScore 0
            (concatMap unbox xs) (concatMap rebox xs)
            (Set.unions $ map alias xs)
            Set.empty
            (entryInfoContext query \\ ctx, ctx \\ entryInfoContext query)
            bs

        ctx = nub $ concat [f c b | (c,v) <- entryInfoContext result, (Var a, b) <- bs, a == v ]
        f c (Var v) = [(c,v)]
        f c (Lit l) = [(c,l) | not $ hasInstance is c l]

        bs = nub $ concatMap bind xs
        badargs = entryInfoArity result - entryInfoArity query


calcScore :: TypeScore -> Int
calcScore t =
    scoreUnbox * length (unbox t) +
    scoreRebox * length (rebox t) +
    scoreAliasFwd * Set.size aliasFwd +
    scoreAliasBwd * Set.size aliasBwd +
    scoreInstanceAdd * length (fst $ badInstance t) +
    scoreInstanceDel * length (snd $ badInstance t) +
    scoreDupVarQuery * f (bind t) +
    scoreDupVarResult * f (map swap $ bind t) +
    scoreRestrict *   length [() | (_, Lit _) <- bind t] +
    scoreUnrestrict * length [() | (Lit _, _) <- bind t]
    where
        (aliasFwd,aliasBwd) = Set.partition isFwd $ alias t
    
        f xs = sum $ map (subtract 1 . length) $ groupFst $ sortFst [(a,b) | (Var a,b) <- xs]
