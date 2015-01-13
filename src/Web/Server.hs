{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- becomes confusing with all the CPP

module Web.Server(server) where

#ifndef MIN_VERSION_wai
#define MIN_VERSION_wai(a,b,c) 1
#endif

import General.Base
import General.Web
import System.FilePath
import CmdLine.All
import Web.Response
import Network.HTTP.Types
import Web.Page
import System.IO.Unsafe(unsafeInterleaveIO)
import Control.Monad.IO.Class
import General.System
import Control.Concurrent
import Control.Exception
import Data.Time.Clock

import Network.Wai
#if MIN_VERSION_wai(2, 0, 0)
import Network.Wai.Internal
#endif
import Network.Wai.Handler.Warp


server :: CmdLine -> IO ()
server q@Server{..} = do
    resp <- respArgs q
    v <- newMVar ()
    putStrLn $ "Starting Hoogle Server on port " ++ show port
    runSettings (setOnException exception $ setPort port defaultSettings)
#if MIN_VERSION_wai(3, 0, 0)
      $ \r sendResponse -> do
#else
      $ \r -> liftIO $ do
#endif
        start <- getCurrentTime
        res <- talk resp q r
        responseEvaluate res
        stop <- getCurrentTime
        let t = floor $ diffUTCTime stop start * 1000
        withMVar v $ const $ putStrLn $ bsUnpack (rawPathInfo r) ++ bsUnpack (rawQueryString r) ++ " ms:" ++ show t
#if MIN_VERSION_wai(3, 0, 0)
        sendResponse res
#else
        return res
#endif


#if MIN_VERSION_wai(2, 0, 0)
exception :: Maybe Request -> SomeException -> IO ()
exception _ e | Just (_ :: InvalidRequest) <- fromException e = return ()
              | otherwise = putStrLn $ "Error: " ++ show e
#else
exception :: SomeException -> IO ()
exception e | Just (_ :: InvalidRequest) <- fromException e = return ()
            | otherwise = putStrLn $ "Error: " ++ show e
#endif


respArgs :: CmdLine -> IO (IO ResponseArgs)
respArgs Server{..} = do
    t <- getTemplate
    if dynamic
        then return $ args t
        else do x <- args t; return $ return x
    where
        getTemplate
            | null template = return $ return defaultTemplates
            | otherwise = do
                let get = do x <- fmap (loadTemplates . unlines) $ mapM readFile' template
                             putStrLn "Templates loaded"
                             return x
                if dynamic then  buffer template get else return get

        modTime ext = unsafeInterleaveIO $ do
            x <- getModificationTime $ resources </> "hoogle" <.> ext
            return $ map (\x -> if isSpace x then '_' else x) $ show x

        args t = do
            css <- modTime "css"; js <- modTime "js"
            t <- t
            return $ responseArgs{updatedCss=css, updatedJs=js, templates=t}


-- | Given a set of paths something relies on, and a value to generate it, return something that generates it minimally
buffer :: [FilePath] -> IO a -> IO (IO a)
buffer files act = do
    val <- act
    ts <- mapM getModificationTime files
    ref <- newMVar (ts,val)
    return $ modifyMVar ref $ \(ts,val) -> do
        ts2 <- mapM getModificationTime files
        if ts == ts2 then return ((ts,val),val) else do
            val <- act
            return ((ts2,val),val)


-- FIXME: Avoid all the conversions to/from LBS
talk :: IO ResponseArgs -> CmdLine -> Request -> IO Response
talk resp Server{..} r@Request{rawPathInfo=path_, rawQueryString=query_}
    | path `elem` ["/","/hoogle"] = do
        let args = parseHttpQueryArgs $ drop 1 query
        cmd <- cmdLineWeb args
        resp <- resp
        r <- response resp cmd{databases=databases}
        if local_ then rewriteFileLinks r else return r
    | path == "/res/search.xml" = serveSearch resources (fmap bsUnpack $ join $ lookup (fromString "domain") $ queryString r)
    | takeDirectory path == "/res" = serveFile True (resources </> takeFileName path) False
    | local_, Just path <- stripPrefix "/file/" path =
        let hasDrive = "/" `isPrefixOf` path && ":" `isPrefixOf` drop 2 path
        in serveFile False (if hasDrive then drop 1 path else path) local_
    | otherwise = return $ responseNotFound $ show path
    where (path,query) = (bsUnpack path_, bsUnpack query_)


serveSearch :: FilePath -> Maybe String -> IO Response
serveSearch resources domain = do
    r <- serveFile True (resources </> "search.xml") False
    case domain of
        Nothing -> return r
        Just x -> responseRewrite (lbsReplace (fromString "http://haskell.org/hoogle/") (fromString x)) r


serveFile :: Bool -> FilePath -> Bool -> IO Response
serveFile cache file rewriteLinks = do
    b <- doesFileExist file
    if not b
        then return $ responseNotFound file
        else (if rewriteLinks then rewriteHaddockFileLinks else return) $ ResponseFile ok200 hdr file Nothing

    where hdr = (hContentType, fromString $ contentExt $ takeExtension file) :
                [(hCacheControl, fromString "max-age=604800" {- 1 week -}) | cache]


rewriteFileLinks :: Response -> IO Response
rewriteFileLinks = responseRewrite $ lbsReplace (fromString "href='file://") (fromString "href='/file/")

replaceLetter :: LBString -> Char -> LBString
replaceLetter lbs letter = lbsReplace (fromString $ "href=\""++[letter]++":") (fromString $ "href=\"/file/"++[letter]++":") lbs

replaceDriveLetters :: LBString -> LBString
replaceDriveLetters lbs = foldl replaceLetter lbs (['A' .. 'Z'] ++ ['a' .. 'z'])

replaceLeadingSlash :: LBString -> LBString
replaceLeadingSlash = lbsReplace (fromString "href=\"/") (fromString "href=\"/file//")

rewriteHaddockFileLinks :: Response -> IO Response
rewriteHaddockFileLinks = responseRewrite $ replaceDriveLetters . replaceLeadingSlash

contentExt ".png" = "image/png"
contentExt ".css" = "text/css"
contentExt ".js" = "text/javascript"
contentExt ".html" = "text/html"
contentExt ".htm" = "text/html"
contentExt ".xml" = "application/opensearchdescription+xml"
contentExt _ = "text/plain"
