
module Web.Action(action) where

import CmdLine.All
import Web.Server
import Web.Response

action :: CmdQuery -> IO ()
action q | Server `elem` queryFlags q = server
action q = putStrLn =<< response q
