
module Web.All(action) where

import CmdLine.All
import General.Web
import Web.Server
import Web.Response


action :: CmdLine -> IO ()
action q@Server{} = server q

-- FIXME: Should use datadir, but not sure how
action q = cgiResponse =<< response "datadir/resources" q
