{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(testMain) where

import Data.Monoid
import Query
import Action.CmdLine


testMain :: CmdLine -> IO ()
testMain Test{} = do
    testQuery
    putStrLn ""


testQuery :: IO ()
testQuery = do
    let a === b | parseQuery a == b = putChar '.'
                | otherwise = error $ show ("testQuery",a,b)
    "" === mempty
