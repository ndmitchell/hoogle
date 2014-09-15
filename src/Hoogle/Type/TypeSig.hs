{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Type.TypeSig where

import Hoogle.Store.All
import General.Base
import Data.Generics.UniplateOn


---------------------------------------------------------------------
-- DATA TYPE WITH CONTEXT


-- FULL TYPE
data TypeSig = TypeSig Constraint Type
               deriving (Eq,Ord,Data,Typeable)

instance NFData TypeSig where
    rnf (TypeSig a b) = rnf (a,b)

type Constraint = [Type]


-- CONSTRICTED TYPE
-- first argument is a list of contexts, (Context,Variable)
type TypeContext = [(String,String)]
data TypeSimp = TypeSimp TypeContext Type
                deriving (Eq,Ord,Data,Typeable)

instance Show TypeSimp where
    show (TypeSimp c t) = show $ TypeSig [TApp (TLit a) [TVar b] | (a,b) <- c] t


---------------------------------------------------------------------
-- DATA TYPES

data Type = TApp Type [Type] -- a list of types, first one being the constructor
          | TLit String -- bound variables, Maybe, ":", "(,)", "(,,)" (tuple)
          | TVar String -- unbound variables, "a"
          | TFun [Type]
          deriving (Eq,Ord,Data,Typeable)

instance NFData Type where
    rnf (TApp a b) = rnf (a,b)
    rnf (TLit a) = rnf a
    rnf (TVar a) = rnf a
    rnf (TFun a) = rnf a

tApp :: Type -> [Type] -> Type
tApp t [] = t
tApp t ts = TApp t ts

ttApp :: Type -> [Type] -> Type
ttApp t [] = t
ttApp (TApp t ts) ts2 = TApp t (ts++ts2)
ttApp t ts = TApp t ts


fromTFun :: Type -> [Type]
fromTFun (TFun x) = x
fromTFun x = [x]


isTLit, isTVar :: Type -> Bool
isTLit TLit{} = True; isTLit _ = False
isTVar TVar{} = True; isTVar _ = False


fromTApp :: Type -> (Type, [Type])
fromTApp (TApp x y) = (x,y)
fromTApp x = (x,[])


isTLitTuple :: String -> Bool
isTLitTuple x = ',' `elem` x


insertTApp, removeTApp :: Type -> Type
insertTApp = transform f
    where
        f (TApp (TApp x []) y) = TApp x y
        f (TApp x y) = TApp x y
        f (TFun x) = TFun x
        f x = TApp x []

removeTApp = transform f
    where
        f (TApp x []) = x
        f x = x


---------------------------------------------------------------------
-- UNIPLATE INSTANCES

instance Uniplate Type where
    uniplate (TApp x xs) = (x:xs, \(x:xs) -> TApp x xs)
    uniplate (TFun xs) = (xs, TFun)
    uniplate x = ([], \[] -> x)

onTypeSig :: BiplateType TypeSig Type
onTypeSig (TypeSig xs x) = (x:xs, \(x:xs) -> TypeSig xs x)

transformSig = transformOn onTypeSig
universeSig = universeOn onTypeSig


variables :: Type -> [String]
variables x = [v | TVar v <- universe x]

variablesSig :: TypeSig -> [String]
variablesSig x = [v | TVar v <- universeSig x]


---------------------------------------------------------------------
-- STORE INSTANCES

instance Store TypeSig where
    put (TypeSig a b) = put2 a b
    get = get2 TypeSig

instance Store Type where
    put (TApp a b) = putByte 0 >> put2 a b
    put (TLit a)   = putByte 1 >> put1 a
    put (TVar a)   = putByte 2 >> put1 a
    put (TFun a)   = putByte 3 >> put1 a

    get = do
        i <- getByte
        case i of
            0 -> get2 TApp
            1 -> get1 TLit
            2 -> get1 TVar
            3 -> get1 TFun


---------------------------------------------------------------------
-- SHOW INSTANCES

showConstraint :: Constraint -> String
showConstraint [] = ""
showConstraint [x] = show x ++ " => "
showConstraint xs = "(" ++ intercalate ", " (map show xs) ++ ") => "


-- TODO: show (TLit ":+:") should be "(:+:)"
instance Show Type where
    showsPrec i x = showString $ f i x
        where
            -- Show lists and tuples specially
            f i (TApp (TLit "[]") [x]) = "[" ++ show x ++ "]"
            f i (TApp (TLit ('(':tup)) xs)
                | not (null tup) && last tup == ')' && all (== ',') (init tup) && length tup == length xs
                = b True $ intercalate ", " $ map show xs

            -- Should parallel lists and unboxed tuples specially
            f i (TApp (TLit "[::]") [x]) = "[:" ++ show x ++ ":]"
            f i (TApp (TLit ('(':'#':tup)) xs)
                | "#)" `isSuffixOf` tup && all (== ',') (drop 2 $ reverse tup) && length tup - 1 == length xs
                = "(# " ++ intercalate ", " (map show xs) ++ " #)"

            
            f i (TLit x) = x
            f i (TVar x) = x
            
            f i (TApp x xs) = b (i > 1) $ unwords $ map (f 2) (x:xs)
            f i (TFun xs)   = b (i > 0) $ intercalate " -> " $ map (f 1) xs
            
            b True x = "(" ++ x ++ ")"
            b False x = x


instance Show TypeSig where
    show (TypeSig x xs) = showConstraint x ++ show xs


-- shows an element within a function
-- to get brackets right after splitFun
showFun :: Type -> String
showFun x = showsPrec 1 x ""


---------------------------------------------------------------------
-- OPERATIONS

normaliseTypeSig :: TypeSig -> TypeSig
normaliseTypeSig = transformOn onTypeSig normaliseType


normaliseType :: Type -> Type
normaliseType = transform f
    where
        f (TApp x []) = x
        f (TApp (TLit "->") xs) = f $ TFun xs
        f (TFun [x]) = x
        f (TFun xs) = TFun $ g xs
        f x = x

        g [] = []
        g [TFun xs] = g xs
        g (x:xs) = x : g xs


splitFun :: Type -> [Type]
splitFun (TFun xs) = xs
splitFun x = [x]


renameVars :: (String -> String) -> TypeSig -> TypeSig
renameVars f = transformOn onTypeSig g
    where
        g (TVar x) = TVar $ f x
        g x = x
