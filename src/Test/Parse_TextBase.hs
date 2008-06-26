
module Test.Parse_TextBase(parse_TextBase) where

import Test.General
import Hoogle.TextBase.All
import Hoogle.TypeSig.All


typ x = case parseTypeSig x of
            Left err -> error $ "parse_TextBase, failed to parse type signature: " ++ show x
            Right y -> y

parse_TextBase = do
    let (===) a b = parseTest parseTextBaseString a (map (flip (,) "") b)

    "" === []
    "-- foo" === []

    "module Foo.Bar.String" === [ItemModule ["Foo","Bar","String"]]
    "module Test" === [ItemModule ["Test"]]

    "class Foo a" === [ItemClass $ typ "Foo a"]
    "class Foo a b" === [ItemClass $ typ "Foo a b"]

    "foo :: Bool" === [ItemFunc "foo" $ typ "Bool"]
    "(++) :: Int" === [ItemFunc "++" $ typ "Int"]
    "++ :: Int" === [ItemFunc "++" $ typ "Int"]
    "@@ :: Int" === [ItemFunc "@@" $ typ "Int"]

    "type Bar a = Foo [a]" === [ItemAlias (typ "Bar a") (typ "Foo [a]")]

    "data Foo a" === [ItemData DataKeyword $ typ "Foo a"]
    "newtype Foo a" === [ItemData NewTypeKeyword $ typ "Foo a"]

    "instance Eq a => Eq [a]" === [ItemInstance $ typ "Eq a => Eq [a]"]

    "@keyword ~" === [ItemAttribute "keyword" "~"]
    "@keyword ->" === [ItemAttribute "keyword" "->"]
    "@keyword module" === [ItemAttribute "keyword" "module"]

    "@package neil" === [ItemAttribute "package" "neil"]
    "@foo bar" === [ItemAttribute "foo" "bar"]
    "@debug" === [ItemAttribute "debug" ""]
