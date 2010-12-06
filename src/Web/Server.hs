{-# LANGUAGE RecordWildCards #-}

module Web.Server(server) where

import General.Base
import General.Web
import CmdLine.All
import Web.Response
import Control.Concurrent
import Control.Exception
import Network
import Network.HTTP
import Network.URI
import Network.Socket
import Data.Time
import System.IO


server :: CmdLine -> IO ()
server q@Server{..} = withSocketsDo $ do
    stop <- httpServer port (talk q)
    putStrLn $ "Started Hoogle Server on port " ++ show port
    b <- hIsClosed stdin
    (if b then forever $ threadDelay maxBound else getChar >> return ()) `finally` stop


-- | Given a port and a handler, return an action to shutdown the server
httpServer :: Int -> (Request String -> IO (Response String)) -> IO (IO ())
httpServer port handler = do
    s <- listenOn $ PortNumber $ fromIntegral port
    forkIO $ forever $ do
        (sock,host) <- Network.Socket.accept s
        bracket
            (socketConnection "" sock)
            close
            (\strm -> do
                start <- getCurrentTime
                res <- receiveHTTP strm
                case res of
                    Left x -> do
                        putStrLn $ "Bad request: " ++ show x
                        respondHTTP strm $ Response (4,0,0) "Bad Request" [] ("400 Bad Request: " ++ show x)
                    Right x -> do
                        respondHTTP strm =<< handler x
                        end <- getCurrentTime
                        let t = floor $ diffUTCTime end start * 1000
                        putStrLn $ "Served in " ++ show t ++ "ms: " ++ unescapeURL (show $ rqURI x)
            )
    return $ sClose s


talk :: CmdLine -> Request String -> IO (Response String)
talk Server{..} Request{rqURI=URI{uriPath=path,uriQuery=query}}
    | path `elem` ["/","/hoogle"] = do
        args <- cmdLineWeb $ parseHttpQueryArgs $ drop 1 query
        response "/res" args{databases=databases}
    | takeDirectory path == "/res" = do
        h <- openBinaryFile (resources </> takeFileName path) ReadMode
        src <- hGetContents h
        return $ Response (2,0,0) "OK"
            [Header HdrContentType $ contentExt $ takeExtension path
            ,Header HdrCacheControl "max-age=604800" {- 1 week -}] src
    | otherwise
        = return $ Response (4,0,4) "Not Found" [] $ "404 Not Found: " ++ show path


contentExt ".png" = "image/png"
contentExt ".css" = "text/css"
contentExt ".js" = "text/javascript"
contentExt _ = "text/plain"
