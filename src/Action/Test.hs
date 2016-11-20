{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Action.Test(actionTest) where

import Query
import Action.CmdLine
import Action.Search
import Action.Server
import Action.Generate
import General.Util
import Input.Item
import Input.Haddock
import System.IO.Extra

import Control.Monad
import Output.Items
import Control.DeepSeq
import Control.Exception


actionTest :: CmdLine -> IO ()
actionTest Test{..} = withBuffering stdout NoBuffering $ withTempFile $ \sample -> do
    putStrLn "Code tests"
    general_util_test
    input_haddock_test
    query_test
    action_server_test_
    putStrLn ""

    putStrLn "Sample database tests"
    actionGenerate defaultGenerate{database=sample, local_=[Just "misc/sample-data"]}
    action_search_test True sample
    action_server_test True sample
    putStrLn ""

    putStrLn "Haskell.org database tests"
    action_search_test False database
    action_server_test False database

    when deep $ withSearch database $ \store -> do
        putStrLn "Deep tests"
        let xs = map targetItem $ listItems store
        evaluate $ rnf xs
        putStrLn $ "Loaded " ++ show (length xs) ++ " items"
