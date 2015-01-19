
module Test.Parse_Query(parse_Query) where

import Prelude()
import General.Base
import Test.General
import Hoogle.Query.All
import Hoogle.Type.All

parse_Query :: IO ()
parse_Query = do
    let (===) = parseTest parseQuery
        q = mempty

    "map" === q{names = ["map"]}
    "#" === q{names = ["#"]}
    "c#" === q{names = ["c#"]}
    "-" === q{names = ["-"]}
    "/" === q{names = ["/"]}
    "->" === q{names = ["->"]}
    "foldl'" === q{names = ["foldl'"]}
    "fold'l" === q{names = ["fold'l"]}
    "Int#" === q{names = ["Int#"]}
    "concat map" === q{names = ["concat","map"]}
    "a -> b" === q{typeSig = Just (TypeSig [] (TFun [TVar "a",TVar "b"]))}
    "(a b)" === q{typeSig = Just (TypeSig [] (TApp (TVar "a") [TVar "b"]))}
    "map :: a -> b" === q{names = ["map"], typeSig = Just (TypeSig [] (TFun [TVar "a",TVar "b"]))}
    "+Data.Map map" === q{scope = [Scope True Module "Data.Map"], names = ["map"]}
    "a -> b +foo" === q{scope = [Scope True Package "foo"], typeSig = Just (TypeSig [] (TFun [TVar "a",TVar "b"]))}
    "a -> b +foo-bar" === q{scope = [Scope True Package "foo-bar"], typeSig = Just (TypeSig [] (TFun [TVar "a",TVar "b"]))}
    "Data.Map.map" === q{scope = [Scope True Module "Data.Map"], names = ["map"]}
    "[a]" === q{typeSig = Just (TypeSig [] (TApp (TLit "[]") [TVar "a"]))}
    "++" === q{names = ["++"]}
    "(++)" === q{names = ["++"]}
    ":+:" === q{names = [":+:"]}
    "bytestring-cvs +hackage" === q{scope=[Scope True Package "hackage"], names=["bytestring-cvs"]}

