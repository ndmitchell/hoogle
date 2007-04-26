
module Hoogle.TypeSig.Type where

import Data.List
import Data.Binary.Defer


data TypeSig = TypeSig Constraint Type
               deriving Eq


type Constraint = [Type]


data Type = TApp Type [Type] -- a list of types, first one being the constructor
          | TLit String -- bound variables, Maybe, ":", "," (tuple), "->" (function)
          | TVar String -- unbound variables, "a"
          | TFun [Type]
          deriving Eq


instance BinaryDefer TypeSig where
    bothDefer = defer [\ ~(TypeSig a b) -> unit TypeSig << a << b]

instance BinaryDefer Type where
    bothDefer = defer [\ ~(TApp a b) -> unit TApp << a << b
                      ,\ ~(TLit a) -> unit TLit << a
                      ,\ ~(TVar a) -> unit TVar << a
                      ,\ ~(TFun a) -> unit TFun << a
                      ]


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
    showsPrec i x = showString $ f i x
        where
            f i (TApp (TLit "[]") [x]) = "[" ++ show x ++ "]"
            f i (TApp (TLit ('(':tup)) xs)
                | not (null tup) && last tup == ')' && all (== ',') (init tup) && length tup == length xs
                = b True $ concat $ intersperse ", " $ map show xs
            
            f i (TLit x) = x
            f i (TVar x) = x
            
            f i (TApp x xs) = b (i > 1) $ concat (intersperse " " $ map (f 2) (x:xs))
            f i (TFun xs)   = b (i > 0) $ concat (intersperse " -> " $ map (f 1) xs)
            
            b True x = "(" ++ x ++ ")"
            b False x = x


instance Show TypeSig where
    show (TypeSig x xs) = showConstraint x ++ show xs


splitFun :: Type -> [Type]
splitFun (TFun xs) = xs
splitFun x = [x]


-- shows an element within a function
-- to get brackets right after splitFun
showFun :: Type -> String
showFun x = showsPrec 1 x ""
