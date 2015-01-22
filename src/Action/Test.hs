{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(testMain) where

import Query
import Action.CmdLine
import Action.Search
import Action.Server
import General.Util
import Input.Type


testMain :: CmdLine -> IO ()
testMain Test{} = do
    Input.Type.test
    Query.test
    Action.Server.test
    testURL
    putStrLn ""


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
    "map package:base" === hackage "base/docs/Prelude.html#v:map"
    "True" === hackage "base/docs/Prelude.html#v:True"
    "Bool" === hackage "base/docs/Prelude.html#t:Bool"
    "String" === hackage "base/docs/Prelude.html#t:String"
    "Ord" === hackage "base/docs/Prelude.html#t:Ord"
    ">>=" === hackage "base/docs/Prelude.html#v:-62--62--61-"
    "foldl'" === hackage "base/docs/Data-List.html#v:foldl-39-"
