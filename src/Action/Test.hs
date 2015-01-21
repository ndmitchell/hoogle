{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(testMain) where

import Query
import Action.CmdLine
import Action.Search
import General.Util
import Input.Type


testMain :: CmdLine -> IO ()
testMain Test{} = do
    testItem
    Query.test
    testURL
    putStrLn ""


testItem :: IO ()
testItem = testing "testItem" $ do
    let a === b | fmap prettyItem (readItem a) == Just b = putChar '.'
                | otherwise = error $ show (a,b,readItem a, fmap prettyItem $ readItem a)
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
    test "module Foo.Bar"
    test "data Char"
    "data Char :: *" === "data Char"
    "newtype ModuleName :: *" === "newtype ModuleName"


testURL :: IO ()
testURL = testing "testURL" $ do
    let a === b = do
            res <- search (Database "output/all") (parseQuery a)
            case res of
                ItemEx{..}:_ | itemURL == b -> putChar '.'
                _ -> error $ show (a, b, take 1 res)
    let hackage x = "https://hackage.haskell.org/package/" ++ x
    "base" === hackage "base"
    "Prelude" === hackage "base/docs/Prelude.html"
    "map" === hackage "base/docs/Prelude.html#v:map"
--    "map package:base" === hackage "base/docs/Prelude.html#v:map"
    "True" === hackage "base/docs/Prelude.html#v:True"
    "Bool" === hackage "base/docs/Prelude.html#t:Bool"
    "String" === hackage "base/docs/Prelude.html#t:String"
    "Ord" === hackage "base/docs/Prelude.html#t:Ord"
    ">>=" === hackage "base/docs/Prelude.html#v:-62--62--61-"
    "foldl'" === hackage "base/docs/Data-List.html#v:foldl-39-"
