
module Web.Action(action) where

import CmdLine.All
import Web.Server
import Web.Response
import General.Web

action :: CmdLine -> IO ()
action Server{} = server
action q = uncurry cgiResponse =<< response q
