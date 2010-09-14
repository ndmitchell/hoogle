
module Web.Server(server) where

import General.Code
import General.Web
import Control.Concurrent
import Control.Exception
import Network


server :: IO ()
server = withSocketsDo $ do
    let port = 4000
    sock <- listenOn $ PortNumber port
    bracket
        (forkIO $ forever $ do
            (h,_,_) <- accept sock
            forkIO $ do
                s <- httpRequest h
                let url = words (head s) !! 1
                print url
                res <- talk url
                httpResponse h res
                hClose h
        )
        (\t -> do killThread t ; putStrLn "killThread")
        (const $ do
            putStrLn $ "Started Hoogle Server on port " ++ show port
            _ <- getChar
            return ())


talk :: String -> IO String
talk x = return $ show x
