
module Web.Server(server) where

import General.Code
import General.Web
import CmdLine.All
import Web.Response
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
                (page,args) <- httpGetArgs h
                print (page,args)
                (heads,body) <- talk page args
                httpResponse h heads body
                hClose h
        )
        (\t -> do killThread t ; putStrLn "killThread")
        (const $ do
            putStrLn $ "Started Hoogle Server on port " ++ show port
            _ <- getChar
            return ())


talk :: String -> [(String,String)] -> IO ([Header], String)
talk page args | page `elem` ["/","/hoogle"] = response =<< cmdLineWeb args
talk page args | takeDirectory page == "/res" = do
    h <- openBinaryFile ("src/res/" ++ takeFileName page) ReadMode
    src <- hGetContents h
    return ([], src)
talk page args = return ([], "unknown")
