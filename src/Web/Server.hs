{-# LANGUAGE RecordWildCards #-}

module Web.Server(server) where

import General.Base
import General.Web
import CmdLine.All
import Web.Response
import Control.Monad.IO.Class
import General.System
import Control.Concurrent

import Network.Wai
import Network.Wai.Handler.Warp


server :: CmdLine -> IO ()
server q@Server{..} = do
    v <- newMVar ()
    putStrLn $ "Starting Hoogle Server on port " ++ show port
    run port $ \r -> liftIO $ do
        withMVar v $ const $ putStrLn $ bsUnpack (pathInfo r) ++ bsUnpack (queryString r)
        talk q r


-- FIXME: Avoid all the conversions to/from LBS
talk :: CmdLine -> Request -> IO Response
talk Server{..} Request{pathInfo=path_, queryString=query_}
    | path `elem` ["/","/hoogle"] = do
        let args = parseHttpQueryArgs $ drop 1 query
        cmd <- cmdLineWeb args
        r <- response "/res" cmd{databases=databases}
        if local_ then rewriteFileLinks r else return r
    | takeDirectory path == "/res" = serveFile True $ resources </> takeFileName path
    | local_ && "/file/" `isPrefixOf` path = serveFile False $ drop 6 path
    | otherwise = return $ responseNotFound $ show path
    where (path,query) = (bsUnpack path_, bsUnpack query_)


serveFile :: Bool -> FilePath -> IO Response
serveFile cache file = do
    b <- doesFileExist file
    return $ if not b
        then responseNotFound file
        else ResponseFile statusOK hdr file
    where hdr = [(hdrContentType, fromString $ contentExt $ takeExtension file)] ++
                [(hdrCacheControl, fromString "max-age=604800" {- 1 week -}) | cache]


rewriteFileLinks :: Response -> IO Response
rewriteFileLinks r = do
    (a,b,c) <- responseFlatten r
    return $ responseLBS a b $ fromString $ f $ lbsUnpack c
    where
        f [] = []
        f o@(x:xs) | "href='file://" `isPrefixOf` o = "href='/file/" ++ f (drop (13+1) o)
                   | otherwise = x : f xs


contentExt ".png" = "image/png"
contentExt ".css" = "text/css"
contentExt ".js" = "text/javascript"
contentExt _ = "text/plain"
