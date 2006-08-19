
module Hoogle.TypeSig.Type where


data TypeSig = TypeSig Constraint Type
               deriving (Eq, Show, Read)


type Constraint = [Type]


data Type = TApp Type [Type] -- a list of types, first one being the constructor
          | TLit String -- bound variables, Maybe, ":", "," (tuple), "->" (function)
          | TVar String -- unbound variables, "a"
          | TFun [Type]
          deriving (Eq, Show, Read)


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
