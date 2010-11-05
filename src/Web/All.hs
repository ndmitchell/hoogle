
module Web.All(action) where

import CmdLine.All
import Web.Server
import Web.Response
import Data.List
import Network.HTTP

action :: CmdLine -> IO ()
action q@Server{} = server q

-- would like to use datadir, but not sure how
action q = do
    res <- response "datadir/resources" q
    putStrLn $ intercalate "\n" $ map show (rspHeaders res) ++ ["",rspBody res]
