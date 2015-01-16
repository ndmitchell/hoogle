{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(testMain) where

import Data.Monoid
import Query
import Action.CmdLine
import Language.Haskell.Exts
import General.Util


testMain :: CmdLine -> IO ()
testMain Test{} = do
    testQuery
    putStrLn ""


testQuery :: IO ()
testQuery = do
    let a === b | parseQuery a == b = putChar '.'
                | otherwise = error $ show ("testQuery",a,parseQuery a,b)
    let a ==$ f | f $ parseQuery a = putChar '.'
                | otherwise = error $ show ("testQuery",a,parseQuery a)
    let typ = fromParseResult . parseTypeWithMode parseMode
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
    "m => c" === q{sig = Just (typ "m => c")}
    "[b ()" === q{sig = Just (typ "[b ()]")}
    "[b (" === q{sig = Just (typ "[b ()]")}
    "_ -> a" ==$ \s -> fmap pretty (sig s) == Just "_ -> a"
    "(a -> b) ->" ==$ \s -> fmap pretty (sig s) ==  Just "(a -> b) -> _"
    "(a -> b) -" ==$ \s -> fmap pretty (sig s) ==  Just "(a -> b) -> _"
    "Monad m => " ==$ \s -> fmap pretty (sig s) == Just "Monad m => _"
