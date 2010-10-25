
module Web.All(action) where

import CmdLine.All
import Web.Server
import Web.Response
import General.Web

action :: CmdLine -> IO ()
action q@Server{} = server q

-- would like to use datadir, but not sure how
action q = do
    res <- response "datadir/resources" q
    uncurry cgiResponse res
