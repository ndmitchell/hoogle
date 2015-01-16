{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main(main) where

import Action.CmdLine
import Action.Generate
import Action.Search
import Action.Server


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

main :: IO ()
main = do
    args <- getCmdLine
    case args of
        Search{} -> searchMain args
        Generate{} -> generateMain args
        Server{} -> spawnMain args
