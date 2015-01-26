{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(testMain) where

import Query
import Action.CmdLine
import Action.Search
import Action.Server
import General.Util
import Input.Type
import Data.List
import System.IO.Extra


testMain :: CmdLine -> IO ()
testMain Test{} = withBuffering stdout NoBuffering $ do
    Input.Type.test
    Query.test
    Action.Server.test
    testURL
    putStrLn ""


testURL :: IO ()
testURL = testing "testURL" $ do
    let a ==$ f = do
            res <- search (Database "output/all") (parseQuery a)
            case res of
                ItemEx{..}:_ | f itemURL -> putChar '.'
                _ -> error $ show (a, take 1 res)
    let a === b = a ==$ (== b)
    let hackage x = "https://hackage.haskell.org/package/" ++ x
    "base" === hackage "base"
    "Prelude" === hackage "base/docs/Prelude.html"
    "map" === hackage "base/docs/Prelude.html#v:map"
    "map package:base" === hackage "base/docs/Prelude.html#v:map"
    "True" === hackage "base/docs/Prelude.html#v:True"
    "Bool" === hackage "base/docs/Prelude.html#t:Bool"
    "String" === hackage "base/docs/Prelude.html#t:String"
    "Ord" === hackage "base/docs/Prelude.html#t:Ord"
    ">>=" === hackage "base/docs/Prelude.html#v:-62--62--61-"
    "foldl'" === hackage "base/docs/Data-List.html#v:foldl-39-"
    "Action package:shake" === "https://hackage.haskell.org/package/shake/docs/Development-Shake.html#t:Action"
    "Action package:shake set:stackage" === "https://hackage.haskell.org/package/shake/docs/Development-Shake.html#t:Action"
    "map -package:base" ==$ \x -> not $ "/base/" `isInfixOf` x
    "<>" === hackage "base/docs/Data-Monoid.html#v:-60--62-"
