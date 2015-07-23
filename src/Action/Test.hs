{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Test(actionTest) where

import Query
import Action.CmdLine
import Action.Search
import Action.Server
import General.Util
import Input.Item
import System.IO.Extra

import Control.Monad
import Output.Items
import Control.DeepSeq
import Control.Exception


actionTest :: CmdLine -> IO ()
actionTest Test{..} = withBuffering stdout NoBuffering $ do
    putStrLn "Quick tests"
    general_util_test
    input_type_test
    query_test
    action_search_test database
    action_server_test
    putStrLn ""
    when deep $ withSearch database $ \store -> do
        putStrLn "Deep tests"
        let xs = map itemItem $ listItems store
        evaluate $ rnf xs
        putStrLn $ "Loaded " ++ show (length xs) ++ " items"

