
module Hoogle.DataBase.Instances(
    Instances, createInstances
    ) where

import Hoogle.Item.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.BinaryDefer

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Binary.Defer
import Data.List

{-
Instances are a little tricky because they are so different on GHC/Hugs/Haskell.

We supports multi-parameter type classes, but only slightly. The rule is:

Class a b === (Class$0 a, Class$1 b)

Instances are stored as:

Class (Data a b) ==> (Class1 a, Class2 a, Class1 b, Class2 b)
Class * ==> (Class1 *, Class2 *)

We are able to capture Haskell 98 classes perfectly, and give a reasonable
interpretation of other things. There are two ways of encoding classes - 'or' and
'and'. Each definition is one instance of an 'or', but may be multiple 'and's.
For example:

Class (Data a Bool) is translated to:
    Class (Data a b) ==> (Class%0 a, Class%1 b)
    Class%0 *
    Class%1 Bool

Class (Data Bool a) is translated to:
    Class%0 Bool
    Class%1 *

This means that Class (Data a b) passes, since we have encoding the 'and' as an 'or'.
This is intentional, but not accurate.

While we allow Class * ==> ... - we ensure that these are all removed at creation time

-}


-- * Data Types

type ClassName = String
type DataName  = String


data Instances = Instances (Map.Map ClassName [ClassName])
                           (Map.Map (ClassName,DataName) [(ClassName,Int)])

data Instance = Always ClassName [ClassName]
              | Sometimes (ClassName,DataName) [(ClassName,Int)]


-- * Instances

instance BinaryDefer Instances where
    bothDefer = defer [\ ~(Instances a b) -> unit Instances << a << b]


commas = concat . intersperse ", "

implies x [] = x
implies x [y] = x ++ " <= " ++ y
implies x ys = x ++ " <= (" ++ commas ys ++ ")"


instance Show Instance where
    show (Always a b) = implies (a ++ " *") (map (++ " *") b)
    show (Sometimes (a,b) c) = implies start end
        where
            start = a ++ " " ++ b
            end = [a ++ " " ++ show b | (a,b) <- c]


instance Show Instances where
    show (Instances a b) = unlines $ "" : "= INSTANCES =" :
        "== Always ==" : (map (show . uncurry Always) (Map.toList a)) ++
        "== Sometimes ==" : (map (show . uncurry Sometimes) (Map.toList b))


-- * Operations


decodeMultiType :: Type -> [Type]
decodeMultiType (TApp (TLit x) xs) | length xs > 1 = zipWith f [0..] xs
    where f n xs = TApp (TLit $ x ++ '$' : show n) [xs]
decodeMultiType x = [x]

decodeMultiTypeSig :: TypeSig -> [TypeSig]
decodeMultiTypeSig (TypeSig con x) = map (TypeSig $ concatMap decodeMultiType con) $ decodeMultiType x



-- * Creation

createInstances :: [Item] -> Instances
createInstances xs = cleanInstances $ Instances
        (Map.fromList [(a,b) | Always a b <- res])
        (Map.fromList [(a,b) | Sometimes a b <- res])
    where
        res = concatMap f sigs
        sigs = [y | Item{itemRest = ItemInstance sig} <- xs, y <- decodeMultiTypeSig sig]

        f (TypeSig con (TApp (TLit cls) [rhs])) =
            case rhs of
                TVar x -> [Always cls [c | TApp (TLit c) [TVar y] <- con, y == x]]
                TApp (TLit dat) vars -> Sometimes (cls,dat) (zip cs is) :
                        concatMap f [TypeSig con (TApp (TLit c) [v]) | (c,v) <- zip cs vars]
                    where
                        is = [0..length vars - 1]
                        cs = map (\i -> cls ++ "%" ++ dat ++ "_" ++ show i) is
                _ -> []


-- normalise rules
-- 1) if there is an Always for a class, and a Sometimes, drop the Sometimes
-- 2) if RHS is an Always, expand it out
-- 3) drop all % classes which are now unreachable
cleanInstances :: Instances -> Instances
cleanInstances = rule3 . rule2 . rule1
    where
        rule1 (Instances every some) = Instances every
            (Map.filterWithKey (\(c,d) v -> not $ c `Map.member` every) some)

        rule2 (Instances every some) = Instances
            (Map.map (concatMap f) every) (Map.map (concatMap (\(x,i) -> map (flip (,) i) (f x))) some)
            where
                f name = case Map.lookup name every of
                    Nothing -> [name]
                    Just y -> concatMap f y

        rule3 (Instances every some) = Instances
            (Map.filterWithKey (\k v -> k `Set.member` reached) every)
            (Map.filterWithKey (\(c,d) v -> c `Set.member` reached) some)
            where
                base = Map.keys every ++ map fst (Map.keys some)
                reached = f Set.empty (filter ('%' `notElem`) base)
                
                f seen (t:odo) | t `Set.member` seen = f seen odo
                               | otherwise = f (Set.insert t seen) (next t ++ odo)
                f seen [] = seen
                
                next x = case Map.lookup x every of
                             Just y -> y
                             _ -> concat [map fst y | ((c,d),y) <- Map.toList some, c == x]

-- * Query

