{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main(main) where

import Data.List
import Data.Tuple.Extra
import System.Environment

import Action.Generate
import Action.Search
import Action.Server


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

main :: IO ()
main = do
    args <- getArgs
    let (pkg,rest) = first (map tail) $ span ("@" `isPrefixOf`) args
    if rest == ["-"] then
        spawnMain pkg
     else if null rest then do
        generateMain pkg
     else
        searchMain pkg rest
