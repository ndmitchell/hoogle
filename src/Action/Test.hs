{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(testMain) where

import Data.Monoid
import Query
import Action.CmdLine
import Language.Haskell.Exts


testMain :: CmdLine -> IO ()
testMain Test{} = do
    testQuery
    putStrLn ""


testQuery :: IO ()
testQuery = do
    let a === b | parseQuery a == b = putChar '.'
                | otherwise = error $ show ("testQuery",a,b)
    let typ = fromParseResult . parseType
    let q = mempty
    "" === mempty
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
    "a -> b" === q{sig = Just (typ "a -> b")}
    "(a b)" === q{sig = Just (typ "(a b)")}
    "map :: a -> b" === q{names = ["map"], sig = Just (typ "a -> b")}
    "+Data.Map map" === q{scope = [Scope True "module" "Data.Map"], names = ["map"]}
    "a -> b package:foo" === q{scope = [Scope True "package" "foo"], sig = Just (typ "a -> b")}
    "a -> b package:foo-bar" === q{scope = [Scope True "package" "foo-bar"], sig = Just (typ "a -> b")}
    "Data.Map.map" === q{scope = [Scope True "module" "Data.Map"], names = ["map"]}
    "[a]" === q{sig = Just (typ "[a]")}
    "++" === q{names = ["++"]}
    "(++)" === q{names = ["++"]}
    ":+:" === q{names = [":+:"]}
    "bytestring-cvs +hackage" === q{scope=[Scope True "package" "hackage"], names=["bytestring-cvs"]}
