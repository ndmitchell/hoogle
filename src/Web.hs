{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings, CPP #-}

module Web(
    Input(..), Output(..), server, downloadFile
    ) where

-- #define PROFILE

-- For some reason, profiling stops working if I import warp
-- Tracked as https://github.com/yesodweb/wai/issues/311
#ifndef PROFILE
import Network.Wai.Handler.Warp hiding (Port)
#endif

import Network.Wai
import Control.DeepSeq
import Control.Exception
import Network.HTTP.Types.Status
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Console.CmdArgs.Verbosity
import Data.Conduit.Binary (sinkFile)
import qualified Network.HTTP.Conduit as C
import qualified Data.Conduit as C
import Network
import System.FilePath


data Input = Input
    {inputURL :: [String]
    ,inputArgs :: [(String, String)]
    ,inputBody :: String
    } deriving Show

data Output
    = OutputString String
    | OutputHTML String
    | OutputFile FilePath
    | OutputError String
    | OutputMissing
      deriving Show

instance NFData Output where
    rnf (OutputString x) = rnf x
    rnf (OutputHTML x) = rnf x
    rnf (OutputFile x) = rnf x
    rnf (OutputError x) = rnf x
    rnf OutputMissing = ()


downloadFile :: FilePath -> String -> IO ()
downloadFile file url = withSocketsDo $ do
    request <- C.parseUrl url
    C.withManager $ \manager -> do
        response <- C.http request manager
        C.responseBody response C.$$+- sinkFile file


server :: Int -> (Input -> IO Output) -> IO ()
#ifdef PROFILE
server port act = return ()
#else
server port act = runSettings (setOnException exception $ setPort port defaultSettings) $ \req reply -> do
    bod <- strictRequestBody req
    whenLoud $ print ("receiving",bod,requestHeaders req,port)
    let pay = Input
            (map Text.unpack $ pathInfo req)
            [(BS.unpack a, maybe "" BS.unpack b) | (a,b) <- queryString req]
            (LBS.unpack bod)
    res <- act pay
    reply $ case res of
        OutputFile file -> responseFile status200
            [("content-type",c) | Just c <- [lookup (takeExtension file) contentType]] file Nothing
        OutputString msg -> responseLBS status200 [] $ LBS.pack msg
        OutputHTML msg -> responseLBS status200 [("content-type","text/html")] $ LBS.pack msg
        OutputError msg -> responseLBS status500 [] $ LBS.pack msg
        OutputMissing -> responseLBS status404 [] $ LBS.pack "Resource not found"

contentType = [(".html","text/html"),(".css","text/css"),(".js","text/javascript")]

exception :: Maybe Request -> SomeException -> IO ()
exception r e
    | Just (_ :: InvalidRequest) <- fromException e = return ()
    | otherwise = putStrLn $ "Error when processing " ++ maybe "Nothing" (show . rawPathInfo) r ++
                             "\n    " ++ show e
#endif
