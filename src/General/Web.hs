{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, RecordWildCards, DeriveFunctor #-}

module General.Web(
    Input(..),
    Output(..), readInput, server
    ) where

import Network.Wai.Handler.Warp hiding (Port, Handle)
import Network.Wai.Handler.WarpTLS

import Action.CmdLine
import Network.Wai.Logger
import Network.Wai
import Control.DeepSeq
import Network.HTTP.Types.Status
import qualified Data.Text as Text
import General.Str
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List.Extra
import Data.Aeson.Encoding
import Data.Char
import Data.String
import Data.Tuple.Extra
import Data.Monoid
import System.FilePath
import Control.Exception.Extra
import System.Time.Extra
import General.Log
import Network.URI
import Prelude


data Input = Input
    {inputURL :: [String]
    ,inputArgs :: [(String, String)]
    } deriving Show

readInput :: String -> Input
readInput (breakOn "?" -> (a,b)) = Input (filter (not . badPath) $ dropWhile null $ splitOn "/" a) $
    filter (not . badArg . fst) $ map (second (unEscapeString . drop1) . breakOn "=") $ splitOn "&" $ drop1 b
    where
        -- avoid "" and ".." in the URLs, since they could be trying to browse on the server
        badPath xs = xs == "" || all (== '.') xs

        badArg xs = xs == "" || any (not . isLower) xs

data Output
    = OutputText LBS.ByteString
    | OutputHTML LBS.ByteString
    | OutputJSON Encoding
    | OutputFail LBS.ByteString
    | OutputFile FilePath
      deriving Show

-- | Force all the output (no delayed exceptions) and produce bytestrings
forceBS :: Output -> LBS.ByteString
forceBS (OutputText x) = force x
forceBS (OutputJSON x) = force $ encodingToLazyByteString x
forceBS (OutputHTML x) = force x
forceBS (OutputFail x) = force x
forceBS (OutputFile x) = rnf x `seq` LBS.empty

instance NFData Output where
    rnf x = forceBS x `seq` ()

server :: Log -> CmdLine -> (Input -> IO Output) -> IO ()
server log Server{..} act = do
    let
        host' = fromString $
                  if host == "" then
                    if local then
                      "127.0.0.1"
                    else
                      "*"
                  else
                    host
        set = setOnExceptionResponse exceptionResponseForDebug
            . setHost host'
            . setPort port $
            defaultSettings
        runServer :: Application -> IO ()
        runServer = if https then runTLS (tlsSettings cert key) set
                             else runSettings set

    logAddMessage log $ "Server starting on port " ++ show port ++ " and host/IP " ++ show host'

    runServer $ \req reply -> do
        putStrLn $ BS.unpack $ rawPathInfo req <> rawQueryString req
        let pay = Input (map Text.unpack $ pathInfo req)
                        [(bstrUnpack a, maybe "" bstrUnpack b) | (a,b) <- queryString req]
        (time,res) <- duration $ try_ $ do s <- act pay; bs <- evaluate $ forceBS s; return (s, bs)
        res <- either (fmap Left . showException) (return . Right) res
        logAddEntry log (showSockAddr $ remoteHost req)
            (BS.unpack $ rawPathInfo req <> rawQueryString req) time (either Just (const Nothing) res)
        case res of
            Left s -> reply $ responseLBS status500 [] $ LBS.pack s
            Right (v, bs) -> reply $ case v of
                OutputFile file -> responseFile status200
                    [("content-type",c) | Just c <- [lookup (takeExtension file) contentType]] file Nothing
                OutputText{} -> responseLBS status200 [("content-type","text/plain")] bs
                OutputJSON{} -> responseLBS status200 [("content-type","application/json"), ("access-control-allow-origin","*")] bs
                OutputFail{} -> responseLBS status500 [("content-type","text/plain")] bs
                OutputHTML{} -> responseLBS status200 [("content-type","text/html")] bs

contentType = [(".html","text/html"),(".css","text/css"),(".js","text/javascript")]
