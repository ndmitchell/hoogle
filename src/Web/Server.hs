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
import General.System


server :: CmdLine -> IO ()
server q@Server{..} = withSocketsDo $ do
    stop <- httpServer port (talk q)
    flip finally stop $ do
        putStrLn $ "Started Hoogle Server on port " ++ show port
        -- must run under nohup, and with input from /dev/null
        shut <- if nostdin then return True else hIsClosed stdin
        eof <- if shut then return True else isEOF
        when eof $ forever $ threadDelay maxBound


-- | Given a port and a handler, return an action to shutdown the server
httpServer :: Int -> (Request String -> IO (Response String)) -> IO (IO ())
httpServer port handler = do
    s <- listenOn $ PortNumber $ fromIntegral port
    forkIO $ forever $ do
        (sock,host) <- Network.Socket.accept s
        bracket (socketConnection "" sock) close $ \strm -> do
            start <- getCurrentTime
            res <- receiveHTTP strm
            case res of
                Left x -> do
                    putStrLn $ "Bad request: " ++ show x
                    respondHTTP strm $ responseBadRequest $ show x
                Right x -> do
                    let msg = unescapeURL $ show $ rqURI x
                    res <- try $ handler x
                    case res of
                        Left e -> do
                            let s = show (e :: SomeException)
                            putStrLn $ "Crash when serving " ++ msg ++ ", " ++ s
                            respondHTTP strm $ responseError s
                        Right r -> do
                            respondHTTP strm r
                            end <- getCurrentTime
                            let t = floor $ diffUTCTime end start * 1000
                            putStrLn $ "Served in " ++ show t ++ "ms: " ++ msg
    return $ sClose s


-- FIXME: This should be in terms of Lazy ByteString's, for higher performance
--        serving of local files
talk :: CmdLine -> Request String -> IO (Response String)
talk Server{..} Request{rqURI=URI{uriPath=path,uriQuery=query}}
    | path `elem` ["/","/hoogle"] = do
        let args = parseHttpQueryArgs $ drop 1 query
        cmd <- cmdLineWeb args
        r <- response "/res" cmd{databases=databases}
        return $ if local_ then fmap rewriteFileLinks r else r
    | takeDirectory path == "/res" = serveFile True $ resources </> takeFileName path
    | local_ && "/file/" `isPrefixOf` path = serveFile False $ drop 6 path
    | otherwise = return $ responseNotFound $ show path


serveFile :: Bool -> FilePath -> IO (Response String)
serveFile cache file = do
    b <- doesFileExist file
    if not b
        then return $ responseNotFound file
        else do
            h <- openBinaryFile file ReadMode
            src <- hGetContents h
            return $ responseOk
                ([Header HdrContentType $ contentExt $ takeExtension file] ++
                 [Header HdrCacheControl "max-age=604800" {- 1 week -} | cache]) src


rewriteFileLinks :: String -> String
rewriteFileLinks x
    | "href='file://" `isPrefixOf` x = "href='/file/" ++ rewriteFileLinks (drop (13+1) x)
    | null x = x
    | otherwise = head x : rewriteFileLinks (tail x)


contentExt ".png" = "image/png"
contentExt ".css" = "text/css"
contentExt ".js" = "text/javascript"
contentExt _ = "text/plain"
