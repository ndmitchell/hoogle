
module Hoogle.TypeSig.Type where


data TypeSig = TypeSig Constraint Type
               deriving (Eq, Show, Read)


type Constraint = [Type]


data Type = TApp Type [Type] -- a list of types, first one being the constructor
          | TLit String -- bound variables, Maybe, ":", "," (tuple), "->" (function)
          | TVar String -- unbound variables, "a"
          | TFun [Type]
          deriving (Eq, Show, Read)
