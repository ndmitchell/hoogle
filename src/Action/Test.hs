{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Action.Test(actionTest) where

import Query
import Action.CmdLine
import Action.Search
import Action.Server
import Action.Generate
import General.Util
import General.Web
import Input.Item
import Input.Haddock
import System.IO.Extra

import Control.Monad
import Output.Items
import Control.DeepSeq
import Control.Exception


actionTest :: Verbosity -> TestOpts -> IO ()
actionTest verbosity TestOpts{..} = withBuffering stdout NoBuffering $ withTempFile $ \sample -> do
    putStrLn "Code tests"
    general_util_test
    general_web_test
    input_haddock_test
    query_test
    action_server_test_
    item_test
    putStrLn ""

    putStrLn "Sample database tests"
    let generateOpts =
            GenerateOpts { database = sample
                         , local_ = ["misc/sample-data"]
                         , download = Nothing
                         , insecure = False
                         , include = []
                         , count = Nothing
                         , haddock = Nothing
                         , debug = False
                         }
    actionGenerate verbosity generateOpts
    action_search_test True sample
    unless disable_network_tests $ action_server_test True sample
    putStrLn ""

    unless disable_network_tests $ do
        putStrLn "Haskell.org database tests"
        action_search_test False database
        action_server_test False database

        when deep $ withSearch database $ \store -> do
            putStrLn "Deep tests"
            let xs = map targetItem $ listItems store
            evaluate $ rnf xs
            putStrLn $ "Loaded " ++ show (length xs) ++ " items"
