
module Test.Parse_Query(parse_Query) where

import Test.General
import Hoogle.Query.All
import Hoogle.TypeSig.All

parse_Query = do
    let (===) = parseTest parseQuery

    "/info" === defaultQuery{flags = [Flag "info" ""]}
    "/count=10" === defaultQuery{flags = [Flag "count" "10"]}
    "map" === defaultQuery{names = ["map"]}
    "concat map" === defaultQuery{names = ["concat","map"]}
    "a -> b" === defaultQuery{typeSig = Just (TypeSig [] (TFun [TVar "a",TVar "b"]))}
    "(a b)" === defaultQuery{typeSig = Just (TypeSig [] (TApp (TVar "a") [TVar "b"]))}
    "map :: a -> b" === defaultQuery{names = ["map"], typeSig = Just (TypeSig [] (TFun [TVar "a",TVar "b"]))}
    "+Data.Map map" === defaultQuery{scope = [PlusModule ["Data","Map"]], names = ["map"]}
    "Data.Map.map" === defaultQuery{scope = [PlusModule ["Data","Map"]], names = ["map"]}
    "[a]" === defaultQuery{typeSig = Just (TypeSig [] (TApp (TLit "[]") [TVar "a"]))}
    "++" === defaultQuery{names = ["++"]}
    "(++)" === defaultQuery{names = ["++"]}
