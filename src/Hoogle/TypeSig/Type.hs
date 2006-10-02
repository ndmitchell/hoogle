
module Hoogle.TypeSig.Type where

import Data.List


data TypeSig = TypeSig Constraint Type
               deriving Eq


type Constraint = [Type]


data Type = TApp Type [Type] -- a list of types, first one being the constructor
          | TLit String -- bound variables, Maybe, ":", "," (tuple), "->" (function)
          | TVar String -- unbound variables, "a"
          | TFun [Type]
          deriving Eq


normaliseTypeSig :: TypeSig -> TypeSig
normaliseTypeSig (TypeSig a b) = TypeSig (map f a) (f b)
    where
        f (TApp x []) = f x
        f (TApp x xs) = TApp (f x) (map f xs)
        
        f (TFun [x]) = f x
        f (TFun xs) = TFun $ g (map f xs)
        
        f x = x
        
        g [] = []
        g [TFun xs] = g xs
        g (x:xs) = x : g xs


showConstraint :: Constraint -> String
showConstraint [] = ""
showConstraint [x] = show x ++ " => "
showConstraint xs = "(" ++ concat (intersperse ", " $ map show xs) ++ ") => "


instance Show Type where
    show (TApp (TLit "[]") [x]) = "[" ++ show x ++ "]"
    show (TApp x xs) = "(" ++ concat (intersperse " " $ map show (x:xs)) ++ ")"
    show (TLit x) = x
    show (TVar x) = x
    show (TFun xs) = "(" ++ concat (intersperse " -> " $ map show xs) ++ ")"


instance Show TypeSig where
    show (TypeSig x xs) = showConstraint x ++ show xs


splitFun :: Type -> [Type]
splitFun (TFun xs) = xs
splitFun x = [x]
