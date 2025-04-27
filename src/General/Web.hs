{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, RecordWildCards, DeriveFunctor #-}

module General.Web(
    Input(..),
    Output(..), readInput, server, general_web_test
    ) where

import Network.Wai.Application.Static
import Network.Wai.Handler.Warp hiding (Port, Handle)
import Network.Wai.Handler.WarpTLS

import Action.CmdLine
import Network.Wai.Logger
import Network.Wai
import Control.DeepSeq
import Network.HTTP.Types (parseQuery, decodePathSegments)
import Network.HTTP.Types.Status
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List.Extra
import Data.Aeson.Encoding
import Data.Char
import Data.String
import Data.Tuple.Extra
import Data.Maybe
import Data.Monoid
import System.FilePath
import Control.Exception.Extra
import System.Time.Extra
import General.Log
import General.Util
import Prelude
import qualified Data.ByteString.UTF8 as UTF8


data Input = Input
    {inputURL :: [String]
    ,inputArgs :: [(String, String)]
    } deriving (Eq, Show)

readInput :: String -> Maybe Input
readInput (breakOn "?" -> (a,b)) =
  if badArgs args then Nothing else Just $ Input path args
  where
    path = parsePath a
    parsePath = map Text.unpack
              . decodePathSegments
              . BS.pack
    args = parseArgs b
    parseArgs = map (UTF8.toString *** maybe "" UTF8.toString)
              . parseQuery
              . UTF8.fromString
    badArgs = not . all (all isLower . fst)

data Output
    = OutputText LBS.ByteString
    | OutputHTML LBS.ByteString
    | OutputJavascript LBS.ByteString
    | OutputJSON Encoding
    | OutputFail LBS.ByteString
    | OutputFile FilePath
    | OutputStaticFile
    -- ^ static file in htmlDir. We fallback to wai-app-static which
    -- gets the filepath from the request so no need to store a filepath
    -- here.
      deriving Show

-- | Force all the output (no delayed exceptions) and produce bytestrings
forceBS :: Output -> LBS.ByteString
forceBS (OutputText x) = force x
forceBS (OutputJSON x) = force $ encodingToLazyByteString x
forceBS (OutputHTML x) = force x
forceBS (OutputJavascript x) = force x
forceBS (OutputFail x) = force x
forceBS (OutputFile x) = rnf x `seq` LBS.empty
forceBS OutputStaticFile = LBS.empty

instance NFData Output where
    rnf x = forceBS x `seq` ()

server :: Log -> CmdLine -> FilePath -> (Input -> IO Output) -> IO ()
server log Server{..} htmlDir act = do
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
        serveStaticFile :: Application
        serveStaticFile = staticApp $ defaultWebAppSettings htmlDir
        secH = if no_security_headers then []
                                      else [
             -- The CSP is giving additional instructions to the browser.
             ("Content-Security-Policy",
              -- For any content type not specifically enumerated in this CSP
              -- (e.g. fonts), the only valid origin is the same as the current
              -- page.
              "default-src 'self';"
              -- As an exception to the default rule, allow scripts from jquery
              -- and the CDN.
              <> " script-src 'self' https://code.jquery.com/ https://rawcdn.githack.com;"
              -- As an exception to the default rule, allow stylesheets from
              -- the CDN. TODO: for now, we are also enabling inline styles,
              -- because it the chosen plugin uses them.
              <> " style-src 'self' 'unsafe-inline' https://rawcdn.githack.com;"
              -- As an exception to the default rule, allow images from the
              -- CDN.
              <> " img-src 'self' https://rawcdn.githack.com;"
              -- Only allow this request in an iframe if the containing page
              -- has the same origin.
              <> " frame-ancestors 'self';"
              -- Forms are only allowed to target addresses under the same
              -- origin as the page.
              <> " form-action 'self';"
              -- Any request originating from this page and specifying http as
              -- its protocol will be automatically upgraded to https.
              <> " upgrade-insecure-requests;"
              -- Do not display http content if the page was loaded under
              -- https.
              <> " block-all-mixed-content"),

             -- Tells the browser this web page should not be rendered inside a
             -- frame, except if the framing page comes from the same origin
             -- (i.e. DNS name + port). This is to thwart invisible, keylogging
             -- framing pages.
             ("X-Frame-Options", "sameorigin"),

             -- Tells browsers to trust the Content-Type header and not try to
             -- otherwise guess at response types. In particular, prevents
             -- dangerous browser behaviour that would execute a file loaded
             -- from a <script> or <style> tag despite not having a
             -- text/javascript or text/css Content-Type.
             ("X-Content-Type-Options", "nosniff"),

             -- Browser should try to detect "reflected" XSS attacks, where
             -- some suspicious payload of the request appears in the response.
             -- How browsers do that is unspecified. On detection, browser
             -- should block the page from rendering at all.
             ("X-XSS-Protection", "1; mode=block"),

             -- Do not include referrer information if user-agent generates a
             -- request from an HTTPS page to an HTTP one. Note: this is
             -- technically redundant as this should be the browser default
             -- behaviour.
             ("Referrer-Policy", "no-referrer-when-downgrade"),

             -- Strict Transport Security (aka HSTS) tells the browser that,
             -- from now on and until max-age seconds have passed, it should
             -- never try to connect to this domain name through unprotected
             -- HTTP. The browser will automatically upgrade any HTTP request
             -- to this domain name to HTTPS, client side, before any network
             -- call happens.
             ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")]

    logAddMessage log $ "Server starting on port " ++ show port ++ " and host/IP " ++ show host'

    runServer $ \req reply -> do
        let pq = BS.unpack $ rawPathInfo req <> rawQueryString req
        putStrLn pq
        (time, res) <- duration $ case readInput pq of
            Nothing -> pure $ Right (OutputFail "", LBS.pack $ "Bad URL: " ++ pq)
            Just pay ->
                handle_ (fmap Left . showException) $ do
                    s <- act pay; bs <- evaluate $ forceBS s; pure $ Right (s, bs)
        logAddEntry log (showSockAddr $ remoteHost req) pq time (either Just (const Nothing) res)
        case res of
            Left s -> reply $ responseLBS status500 [] $ LBS.pack s
            Right (v, bs) -> case v of
                OutputFile file -> reply $ responseFile status200
                    ([("content-type",c) | Just c <- [lookup (takeExtension file) contentType]] ++ secH) file Nothing
                OutputText{} -> reply $ responseLBS status200 (("content-type","text/plain") : secH) bs
                OutputJSON{} -> reply $ responseLBS status200 (("content-type","application/json") : ("access-control-allow-origin","*") : secH) bs
                OutputFail{} -> reply $ responseLBS status400 (("content-type","text/plain") : secH) bs
                OutputHTML{} -> reply $ responseLBS status200 (("content-type","text/html") : secH) bs
                OutputJavascript{} -> reply $ responseLBS status200 (("content-type","text/javascript") : secH) bs
                OutputStaticFile -> serveStaticFile req reply

contentType = [(".html","text/html"),(".css","text/css"),(".js","text/javascript")]

general_web_test :: IO ()
general_web_test = do
    testing "General.Web.readInput" $ do
        let a === b = if a == b then putChar '.' else errorIO $ show (a,b)
        readInput "abc" === Just (Input ["abc"] [])
        readInput "/abc" === Just (Input ["abc"] [])
        readInput "/abc/" === Just (Input ["abc", ""] [])
        readInput "abc?ab=cd&ef=gh" === Just (Input ["abc"] [("ab", "cd"), ("ef", "gh")])
        readInput "%2fabc" === Just (Input ["/abc"] [])
        readInput "%2F" === Just (Input ["/"] [])
        readInput "def%2fabc" === Just (Input ["def/abc"] [])
        readInput "." === Just (Input ["."] [])
        readInput ".." === Just (Input [".."] [])
        readInput "..a" === Just (Input ["..a"] [])
        readInput "../a" === Just (Input ["..", "a"] [])
        readInput "a/../a" === Just (Input ["a", "..", "a"] [])
        readInput "%2e" === Just (Input ["."] [])
        readInput "%2E" === Just (Input ["."] [])
