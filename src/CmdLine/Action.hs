
module CmdLine.Action(actionCmdLine) where

import CmdLine.Query
import Text.ParserCombinators.Parsec
import System.Exit


actionCmdLine :: CmdQuery -> IO ()
actionCmdLine CmdQuery{queryText = text, query = Left err} = do
    putStrLn $ msg ++ show err
    exitFailure
    where
        msg = unlines ["Parse error:", "  " ++ text,
                        replicate (sourceColumn (errorPos err) + 1) ' ' ++ "^"]


actionCmdLine _ = error "todo"
