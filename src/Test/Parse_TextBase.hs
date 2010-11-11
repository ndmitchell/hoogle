
module Test.Parse_TextBase(parse_TextBase) where

import Test.General
--import Hoogle.TextBase.All
--import Hoogle.Query.All
--import Data.Maybe
import Debug.Trace

{-
typ x = case parseQuery $ ":: " ++ x of
            Left err -> error $ "parse_TextBase, failed to parse type signature: " ++ show x
            Right y -> fromMaybe (error $ "No type found in parse_TextBase for: " ++ show x) $ typeSig y
-}

parse_TextBase :: Test ()
parse_TextBase = do
    trace "Warning: Currently all TextBase parse things are disabled" $ pass
    {-
    let (===) a b = parseTest2 parseTextBase a (map (\x -> ("","",x)) b)

    "" === []
    "-- foo" === []

    "module Foo.Bar.String" === [ItemModule ["Foo","Bar","String"]]
    "module Test" === [ItemModule ["Test"]]

    "class Foo a" === [ItemClass $ typ "Foo a"]
    "class Foo a b" === [ItemClass $ typ "Foo a b"]

    "foo :: Bool" === [ItemFunc "foo" $ typ "Bool"]
    "True :: Bool" === [ItemFunc "True" $ typ "Bool"]
    "(++) :: Int" === [ItemFunc "++" $ typ "Int"]
    "(@@) :: Int" === [ItemFunc "@@" $ typ "Int"]

    "type Bar a = Foo [a]" === [ItemAlias (typ "Bar a") (typ "Foo [a]")]

    "data (:+:) a b" === [ItemData DataKeyword $ typ "(:+:) a b"]

    "data Foo a" === [ItemData DataKeyword $ typ "Foo a"]
    "newtype Foo a" === [ItemData NewTypeKeyword $ typ "Foo a"]

    "instance Eq a => Eq [a]" === [ItemInstance $ typ "Eq a => Eq [a]"]

    "@keyword ~" === [ItemAttribute "keyword" "~"]
    "@keyword ->" === [ItemAttribute "keyword" "->"]
    "@keyword module" === [ItemAttribute "keyword" "module"]

    "@package neil" === [ItemAttribute "package" "neil"]
    "@foo bar" === [ItemAttribute "foo" "bar"]
    "@debug" === [ItemAttribute "debug" ""]
-}
