
module Hoogle.DataBase.TypeSearch.TypeScore(TypeScore) where

import General.Code
import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.TypeSig.All
import qualified Data.Set as Set


data FwdBwd a = Fwd a | Bwd a
     deriving (Eq,Ord,Show)
     
{-
If a and b are restricted to M, it may be that a=b, therefore one restrict
penalty is appropriate. Therefore restrictions are accumulated and only
one case of each is added, until the final binding set is known.
-}


data TypeScore = TypeScore
    {score :: Int
    ,unbox :: [String], rebox :: [String]
    ,alias :: Set.Set (FwdBwd String)
    ,restrict :: Set.Set (FwdBwd String)
    ,badInstanceQuery :: TypeContext, badInstanceResult :: TypeContext
    ,badBind :: [([String],[String])]
    ,bind :: [Bind]
    }
    deriving Show

instance Eq TypeScore where
    (==) = (==) `on` score

instance Ord TypeScore where
    compare = compare `on` score


data Bind = VarVar String String
          | VarLit String String
          | LitVar String String
          deriving (Eq,Show)


emptyTypeScore :: TypeScore
emptyTypeScore = TypeScore 0 [] [] Set.empty Set.empty [] [] [] []


add v t = t{score = score t + v}


-- if adding would bind two literals together, then fail
addCost :: Cost -> TypeScore -> Maybe TypeScore
addCost (CostAlias a)               t = addAlias scoreAliasFwd (Fwd a) t
addCost (CostReverse (CostAlias a)) t = addAlias scoreAliasBwd (Bwd a) t

addCost (CostUnbox a) t = Just $ add scoreUnbox $ t{unbox = a : unbox t}
addCost (CostReverse (CostUnbox a)) t = Just $ add scoreRebox t{rebox = a : rebox t}

-- A |--> b, always restricts to a fresh variable on RHS
addCost (CostRestrict a b) t = addRestrict scoreRestrict (Fwd a) (LitVar a b) t

-- b |--> A
addCost (CostReverse (CostRestrict a b)) t
    | or [l /= a | VarLit v l <- bind t, v == b] = Nothing -- b |--> (A and C)
    | otherwise = addRestrict scoreUnrestrict (Bwd a) (VarLit b a) t


addAlias score x t
    | x `Set.member` alias t = Just t
    | otherwise = Just $ add score $ t{alias = Set.insert x (alias t)}

addRestrict score x b t
    | x `Set.member` restrict t = Just t{bind = b `consNub` bind t}
    | otherwise = Just $ add score $ t
        {bind = b : bind t
        ,restrict = Set.insert x (restrict t)}



scoreBindingUnique :: Binding -> TypeScore -> TypeScore
scoreBindingUnique = undefined


mergeTypeScores :: [TypeScore] -> TypeScore
mergeTypeScores = undefined
