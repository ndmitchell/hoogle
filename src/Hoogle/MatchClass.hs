{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

module Hoogle.MatchClass(
    ClassTable,
    buildClass,
    lookupClass
    ) where

import Hoogle.Result
import Hoogle.TypeSig
import Hoogle.General

import Data.Maybe
import qualified Data.Map as Map


data ClassTable = ClassTable (Map.Map String [TypeMatch])
                  deriving Show


data TypeMatch = TypeMatch [Type] Constraint
                 deriving Show


-- item should only be instances
buildClass :: [Item] -> ClassTable
buildClass xs = ClassTable $ foldr add Map.empty (map f xs)
    where
        f (Instance (con, TList (TLit name:typs))) = (name, TypeMatch (f typs) (f con))
            where
                free = zip (allTVar (TList typs)) [0..]
                f = map (mapUnbound g)
                g x = TNum $ fromJust $ lookup x free
        
        add (name, value) mp = case Map.lookup name mp of
                                   Just x -> Map.insert name (value:x) mp
                                   Nothing -> Map.insert name [value] mp



-- return ClassMinor if something is minorly wrong, i.e. no Show for a
-- return ClassMajor for bigger errors, no FooBar for a
lookupClass :: ClassTable -> Constraint -> String -> [Type] -> Maybe [MatchAmount]
lookupClass ct given check xs | all isTVar xs =
    if any (== TList (TLit check : xs)) given then Just []
    else if check `elem` ["Eq","Show","Ord"] then Just [ClassMinor]
    else Just [ClassMajor]

-- either reduce or perish!
lookupClass c@(ClassTable ct) given check typ =
        case mapMaybe f res of
            [] -> Just [ClassMajor]
            (x:xs) -> Just x
    where
        ltyp = length typ
        res = Map.findWithDefault [] check ct
        
        f (TypeMatch typ2 con)
                | length typ2 == ltyp && isJust unifs && isJust res
                = Just $ fromJust res
            where 
                unifs = concatIdMaybeAll $ zipWith g typ2 typ
                cons2 = map (mapNumber (\x -> fromJust $ lookup x (fromJust unifs))) con
                res = concatMapMaybeAll ren cons2
                
                ren (TList (TLit x:xs)) = lookupClass c given x xs
                
        
        f _ = Nothing
        
        -- type on the left must have the numbers in
        g :: Type -> Type -> Maybe [(Int, Type)]
        g (TNum x) y = Just [(x, y)]
        g (TLit x) (TLit y) | x == y = Just []
        g (TList x) (TList y) | length x == length y = concatIdMaybeAll $ zipWith g x y
        g _ _ = Nothing
        
        