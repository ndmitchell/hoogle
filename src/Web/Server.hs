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
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS


server :: CmdLine -> IO ()
server q@Server{..} = do
    v <- newMVar ()
    putStrLn $ "Starting Hoogle Server on port " ++ show port
    let err x = putStrLn $ "Error: " ++ show x
    runEx err port $ \r -> liftIO $ do
        withMVar v $ const $ putStrLn $ bsUnpack (pathInfo r) ++ bsUnpack (queryString r)
        talk q r


-- FIXME: Avoid all the conversions to/from LBS
talk :: CmdLine -> Request -> IO Response
talk Server{..} Request{pathInfo=path_, queryString=query_}
    | path `elem` ["/","/hoogle"] = do
        let args = parseHttpQueryArgs $ drop 1 query
        cmd <- cmdLineWeb args
        r <- response responseArgs cmd{databases=databases}
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
    let res = LBS.fromChunks $ f $ BS.concat $ LBS.toChunks c
    return $ responseLBS a b res
    where
        f x | BS.null b = [a]
            | otherwise = a : rep : f (BS.drop nfind b) 
            where (a,b) = BS.breakSubstring find x

        find = fromString "href='file://"
        rep = fromString "href='/file/"
        nfind = BS.length find


contentExt ".png" = "image/png"
contentExt ".css" = "text/css"
contentExt ".js" = "text/javascript"
contentExt ".html" = "text/html"
contentExt ".htm" = "text/html"
contentExt _ = "text/plain"
