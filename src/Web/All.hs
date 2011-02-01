
module Web.All(action) where

import CmdLine.All
import General.Web
import Web.Server
import Web.Response


action :: CmdLine -> IO ()
action q@Server{} = server q
action q = cgiResponse =<< response responseArgs q
