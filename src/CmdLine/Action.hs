
module CmdLine.Action(actionCmdLine) where

import CmdLine.Flag
import CmdLine.Query
import Data.List
import Text.ParserCombinators.Parsec
import System.Exit


actionCmdLine :: CmdQuery -> IO ()

actionCmdLine CmdQuery{queryText = text, query = Left err} = do
    putStrLn $ msg ++ show err
    exitFailure
    where
        msg = unlines ["Parse error:", "  " ++ text,
                        replicate (sourceColumn (errorPos err) + 1) ' ' ++ "^"]


actionCmdLine q | not $ null $ queryBadFlags q = do
    putStr $ unlines ["Unrecognised or malformed flags:"
                     ,"  " ++ concat (intersperse ", " $ map show $ queryBadFlags q)
                     ,"For details on correct flags pass --help"]
    exitFailure


actionCmdLine q | Version `elem` queryFlags q = putStr $ unlines
    ["Hoogle - (C) Neil Mitchell 2004-2008, University of York, UK"
    ,"Version 4.0 pre"]


actionCmdLine q | Help `elem` queryFlags q =
    putStrLn "Go to the website for help, http://haskell.org/hoogle/"


actionCmdLine _ = error "todo"
