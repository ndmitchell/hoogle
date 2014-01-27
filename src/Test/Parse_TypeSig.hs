
module Test.Parse_TypeSig(parse_TypeSig) where

import Test.General
import Data.Maybe
import Hoogle.Type.All
import Hoogle.Query.All


parse_TypeSig :: IO ()
parse_TypeSig = do
    let parseTypeSig x = either Left (Right . fromMaybe (error $ "Couldn't find type in: " ++ x) . typeSig) $ parseQuery (":: " ++ x)
    let (===) = parseTest parseTypeSig

    -- really basic stuff
    "a" === TypeSig [] (TVar "a")
    "a_" === TypeSig [] (TVar "a_")
    "_" === TypeSig [] (TVar "_")
    "_a" === TypeSig [] (TVar "_a")
    "A" === TypeSig [] (TLit "A")
    "A_" === TypeSig [] (TLit "A_")
    "m a" === TypeSig [] (TApp (TVar "m") [TVar "a"])
    "M a b" === TypeSig [] (TApp (TLit "M") [TVar "a",TVar "b"])

    -- lists and tuples
    "[a]" === TypeSig [] (TApp (TLit "[]") [TVar "a"])
    "[] a" === TypeSig [] (TApp (TLit "[]") [TVar "a"])
    "()" === TypeSig [] (TLit "()")
    "(a)" === TypeSig [] (TVar "a")
    "(a,b)" === TypeSig [] (TApp (TLit "(,)") [TVar "a",TVar "b"])
    "(,) a b" === TypeSig [] (TApp (TLit "(,)") [TVar "a",TVar "b"])
    "Foo [a]" === TypeSig [] (TApp (TLit "Foo") [TApp (TLit "[]") [TVar "a"]])

    -- functions
    "(->)" === TypeSig [] (TLit "->")
    "a -> b" === TypeSig [] (TFun [TVar "a",TVar "b"])
    "a->b->c" === TypeSig [] (TFun [TVar "a",TVar "b",TVar "c"])
    "a -> (b -> c)" === TypeSig [] (TFun [TVar "a",TVar "b",TVar "c"])
    "(a -> b) -> c" === TypeSig [] (TFun [TFun [TVar "a",TVar "b"],TVar "c"])
    "M (a b) c" === TypeSig [] (TApp (TLit "M") [TApp (TVar "a") [TVar "b"],TVar "c"])
    "(-#)" === TypeSig [] (TLit "-#")
    "a -# b" === TypeSig [] (TApp (TLit "-#") [TVar "a",TVar "b"])

    -- classes
    "Eq a => a" === TypeSig [TApp (TLit "Eq") [TVar "a"]] (TVar "a")
    "Class a b => a b" === TypeSig [TApp (TLit "Class") [TVar "a",TVar "b"]] (TApp (TVar "a") [TVar "b"])
    "(Ord a, Eq b) => a -> b" === TypeSig [TApp (TLit "Ord") [TVar "a"],TApp (TLit "Eq") [TVar "b"]] (TFun [TVar "a",TVar "b"])

    -- forall
    "forall a . a -> a" === TypeSig [] (TFun [TVar "a", TVar "a"])
    "forall a b . a -> a" === TypeSig [] (TFun [TVar "a", TVar "a"])
    "(forall a . a -> a) -> b -> b" === TypeSig [] (TFun [TFun [TVar "a", TVar "a"], TVar "b", TVar "b"])
    "(forall a . Data a => a -> a) -> b -> b" === TypeSig [] (TFun [TFun [TVar "a", TVar "a"], TVar "b", TVar "b"])

    -- type operators
    "(:+:) a b" === TypeSig [] (TApp (TLit ":+:") [TVar "a", TVar "b"])
    "(+++) a b" === TypeSig [] (TApp (TLit "+++") [TVar "a", TVar "b"])
    "a :+: b" === TypeSig [] (TApp (TLit ":+:") [TVar "a", TVar "b"])
    "a +++ b" === TypeSig [] (TApp (TLit "+++") [TVar "a", TVar "b"])

    -- unboxed values
    "Int#" === TypeSig [] (TLit "Int#")
    "State# RealWorld" === TypeSig [] (TApp (TLit "State#") [TLit "RealWorld"])
    "(# a, b #)" === TypeSig [] (TApp (TLit "(#,#)") [TVar "a",TVar "b"])
    "(#,#) a b" === TypeSig [] (TApp (TLit "(#,#)") [TVar "a",TVar "b"])

    -- parallel arrays
    "[:a:]" === TypeSig [] (TApp (TLit "[::]") [TVar "a"])
    "[::] a" === TypeSig [] (TApp (TLit "[::]") [TVar "a"])

    -- real examples
    "(a -> b) -> [a] -> [b]" === TypeSig [] (TFun [TFun [TVar "a",TVar "b"],TApp (TLit "[]") [TVar "a"],TApp (TLit "[]") [TVar "b"]])
    "Monad a => (b -> a c) -> [b] -> a [c]" === TypeSig [TApp (TLit "Monad") [TVar "a"]] (TFun [TFun [TVar "b",TApp (TVar "a") [TVar "c"]],TApp (TLit "[]") [TVar "b"],TApp (TVar "a") [TApp (TLit "[]") [TVar "c"]]])
    "GraphM m gr => Node -> m (gr a b) -> m (Maybe [Node])" === TypeSig [TApp (TLit "GraphM") [TVar "m",TVar "gr"]] (TFun [TLit "Node",TApp (TVar "m") [TApp (TVar "gr") [TVar "a",TVar "b"]],TApp (TVar "m") [TApp (TLit "Maybe") [TApp (TLit "[]") [TLit "Node"]]]])
    "Ix a => Array a b -> a -> b" === TypeSig [TApp (TLit "Ix") [TVar "a"]] (TFun [TApp (TLit "Array") [TVar "a",TVar "b"],TVar "a",TVar "b"])

