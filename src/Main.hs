{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Main(main) where

import Action.CmdLine
import Action.Generate
import Action.Search
import Action.Server
import Action.Test
import System.IO


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getCmdLine
    case args of
        Search{} -> actionSearch args
        Generate{} -> actionGenerate args
        Server{} -> actionServer args
        Test{} -> actionTest args
        Replay{} -> actionReplay args
