{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

{-|
    Data structures for each of the type signatures
-}

module Hoogle.TypeSig where

import Data.List



type ModuleName = [String]

data Item = Module ModuleName
          | Class {typ :: ConType}
          | Func {name :: String, typ :: ConType}
          | TypeAlias {name :: String, args :: [String], typ :: ConType}
          | Data {new :: Bool, typ :: ConType}
          | Instance {typ :: ConType}
          | Keyword {name :: String}


instance Show Item where
    show (Module x) = "module " ++ concat (intersperse "." x)
    show (Class typ) = "class " ++ showConType typ
    show (Func name typ) = name ++ " :: " ++ showConType typ
    show (TypeAlias name args typ) = name ++ concatMap (' ':) args ++ " = " ++ showConType typ
    show (Data new typ) = (if new then "newtype" else "data") ++ " " ++ showConType typ
    show (Instance typ) = "instance " ++ showConType typ
          


isTypeAlias (TypeAlias{}) = True; isTypeAlias _ = False
isInstance  (Instance {}) = True; isInstance  _ = False
isFunc      (Func     {}) = True; isFunc      _ = False


asString :: Item -> String
asString (Module x) = concat $ intersperse "." x
asString (Func {name=x}) = x
asString (TypeAlias {name=x}) = x
asString (Data {typ=(_,x)}) = typeName x
asString (Class {typ=(_,x)}) = typeName x
asString (Keyword {name=x}) = x


asType :: Item -> String
asType (Module x) = "module"
asType x = showConType $ typ x


typeName :: Type -> String
typeName (TList (x:xs)) = typeName x
typeName (TLit x) = x


type ConType = (Constraint, Type)


type Constraint = [Type]


data Type = TList [Type] -- a list of types, first one being the constructor
          | TLit String -- bound variables, Maybe, ":", "," (tuple), "->" (function)
          | TVar String -- unbound variables, "a"
          | TNum Int -- a point to a number
          deriving (Show, Eq)
          

isTVar (TVar _) = True
isTVar _ = False


allTVar :: Type -> [String]
allTVar x = nub $ f x
    where
        f (TList xs) = concatMap f xs
        f (TVar  xs) = [xs]
        f _          = []

          
mapUnbound :: (String -> Type) -> Type -> Type
mapUnbound f (TList xs) = TList (map (mapUnbound f) xs)
mapUnbound f (TVar  x ) = f x
mapUnbound f x          = x

mapNumber :: (Int -> Type) -> Type -> Type
mapNumber f (TList xs) = TList (map (mapNumber f) xs)
mapNumber f (TNum  x ) = f x
mapNumber f x          = x


showCon :: Constraint -> String
showCon []  = ""
showCon [c] = showType c ++ " => "
showCon cs  = "(" ++ concat (intersperse ", " (map showType cs)) ++ ") => "


showConType :: ConType -> String
showConType (c, x) = showCon c ++ showType x


showModuleName :: ModuleName -> String
showModuleName x = concat $ intersperse "." x


showType :: Type -> String
showType = showTypePrec 0

showTypePrec :: Int -> Type -> String
showTypePrec _ (TLit x) = x
showTypePrec _ (TVar x) = x
showTypePrec _ (TNum x) = show x
showTypePrec p (TList (x:xs)) = case x of
        TLit "[]" -> "[" ++ showTypePrec 0 (head xs) ++ "]"
        TLit ","  -> showTypeList True  ", "   0 xs
        TLit "->" -> showTypeList (p>0) " -> " 1 xs
        _         -> showTypeList (p>1) " "    2 (x:xs)
    where
        showTypeList p mid nxt xs = parens p $ concat $ intersperse mid $ map (showTypePrec nxt) xs
        parens True  x = "(" ++ x ++ ")"
        parens False x = x

