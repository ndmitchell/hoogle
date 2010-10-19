{-# LANGUAGE RecordWildCards #-}

module Web.Server(server) where

import General.Code
import General.Web
import CmdLine.All
import Web.Response
import Control.Concurrent
import Control.Exception
import Network


server :: CmdLine -> IO ()
server q@Server{..} = withSocketsDo $ do
    sock <- listenOn $ PortNumber $ fromIntegral port
    bracket
        (forkIO $ forever $ do
            (h,_,_) <- accept sock
            forkIO $ do
                (page,args) <- httpGetArgs h
                print (page,args)
                (heads,body) <- talk q page args
                httpResponse h heads body
                hClose h
        )
        (\t -> do killThread t ; putStrLn "killThread")
        (const $ do
            putStrLn $ "Started Hoogle Server on port " ++ show port
            _ <- getChar
            return ())


talk :: CmdLine -> String -> [(String,String)] -> IO ([Header], String)
talk Server{..} page args | page `elem` ["/","/hoogle"] = do
    args <- cmdLineWeb args
    response "/res" args{databases=databases}
talk Server{..} page args | takeDirectory page == "/res" = do
    h <- openBinaryFile (resources </> takeFileName page) ReadMode
    src <- hGetContents h
    return ([], src)
talk _ page args = return ([], "unknown")
