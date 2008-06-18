
module CmdLine.Search(actionSearch) where

import CmdLine.Flag
import Hoogle.Query.All


actionSearch :: [CmdFlag] -> Query -> IO ()
actionSearch _ _ = error "todo"
