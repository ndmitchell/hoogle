{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(testMain) where

import Query
import Action.CmdLine
import Language.Haskell.Exts
import General.Util
import Data.List
import Input.Type


testMain :: CmdLine -> IO ()
testMain Test{} = do
    testItem
    testQuery
    putStrLn ""

testing :: String -> IO () -> IO ()
testing name act = do putStr $ "Test " ++ name ++ " "; act; putStrLn ""


testItem :: IO ()
testItem = testing "testItem" $ do
    let a === b | fmap prettyItem (readItem a) == Just b = putChar '.'
                | otherwise = error $ show ("testItem",a,b,readItem a, fmap prettyItem $ readItem a)
    let test a = a === a
    test "type FilePath = [Char]"
    test "data Maybe a"
    test "Nothing :: Maybe a"
    test "Just :: a -> Maybe a"
    test "newtype Identity a"
    test "foo :: Int# -> b"
    test "(,,) :: a -> b -> c -> (a, b, c)"
    test "reverse :: [a] -> [a]"
    test "reverse :: [:a:] -> [:a:]"


testQuery :: IO ()
testQuery = testing "testQuery" $ do
    let names x (bad,q) = (["bad name, expected " ++ show x | queryName q /= x] ++ bad, q{queryName=[]})
        name x = names [x]
        typ x (bad,q) = (["bad type, expected " ++ show x | queryType q /= Just (fromParseResult $ parseTypeWithMode parseMode x)] ++ bad, q{queryType=Nothing})
        typpp x (bad,q) = (["bad type, expected " ++ show x | fmap pretty (queryType q) /= Just x], q)
        scopes xs (bad,q) = (["bad scope, expected " ++ show xs | not $ xs `isPrefixOf` queryScope q] ++ bad, q{queryScope=drop (length xs) $ queryScope q})
        scope b c v = scopes [Scope b c v]
    let infixl 0 ===
        a === f | bad@(_:_) <- fst $ f ([], q) = error $ show ("testQuery",a,q,bad :: [String])
                | otherwise = putChar '.'
            where q = parseQuery a

    "" === id
    "map" === name "map"
    "#" === name "#"
    "c#" === name "c#"
    "-" === name "-"
    "/" === name "/"
    "->" === name "->"
    "foldl'" === name "foldl'"
    "fold'l" === name "fold'l"
    "Int#" === name "Int#"
    "concat map" === names ["concat","map"]
    "a -> b" === typ "a -> b"
    "(a b)" === typ "(a b)"
    "map :: a -> b" === typ "a -> b"
    "+Data.Map map" === scope True "module" "Data.Map" . name "map"
    "a -> b package:foo" === scope True "package" "foo" . typ "a -> b"
    "a -> b package:foo-bar" === scope True "package" "foo-bar" . typ "a -> b"
    "Data.Map.map" === scope True "module" "Data.Map" . name "map"
    "[a]" === typ "[a]"
    "++" === name "++"
    "(++)" === name "++"
    ":+:" === name ":+:"
    "bytestring-cvs +hackage" === scope True "package" "hackage" . name "bytestring-cvs"
    "m => c" === typ "m => c"
    "[b ()" === typ "[b ()]"
    "[b (" === typ "[b ()]"
    "_ -> a" === typpp "_ -> a"
    "(a -> b) ->" === typpp "(a -> b) -> _"
    "(a -> b) -" === typpp "(a -> b) -> _"
    "Monad m => " === typpp "Monad m => _"
    "map is:exact" === name "map" . scope True "is" "exact"
    "sort set:hackage" === name "sort" . scope True "set" "hackage"
    "package:bytestring-csv" === scope True "package" "bytestring-csv"
