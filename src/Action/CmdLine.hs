{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse #-}

module Action.CmdLine(
    CmdLine(..), Language(..),
    getCmdLine, defaultDatabaseLang,
    defaultGenerate,
    whenLoud, whenNormal
    ) where

import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.FilePath
import Data.List.Extra
import Data.Version
import General.Util
import Paths_hoogle(version)

data Language = Haskell | Frege deriving (Data,Typeable,Show,Eq,Enum,Bounded)

data CmdLine
    = Search
        {color :: Maybe Bool
        ,link :: Bool
        ,numbers :: Bool
        ,info :: Bool
        ,database :: FilePath
        ,count :: Int
        ,query :: [String]
        ,repeat_ :: Int
        ,language :: Language
        ,compare_ :: [String]
        }
    | Generate
        {download :: Maybe Bool
        ,database :: FilePath
        ,insecure :: Bool
        ,include :: [String]
        ,count :: Int
        ,local_ :: [FilePath]
        ,haddock :: Maybe FilePath
        ,debug :: Bool
        ,language :: Language
        }
    | Server
        {port :: Int
        ,database :: FilePath
        ,cdn :: String
        ,logs :: FilePath
        ,local :: Bool
        ,haddock :: Maybe FilePath
        ,links :: Bool
        ,language :: Language
        ,scope :: String
        ,home :: String
        ,host :: String
        ,https :: Bool
        ,cert :: FilePath
        ,key :: FilePath
        ,datadir :: Maybe FilePath
        ,no_security_headers :: Bool
        }
    | Replay
        {logs :: FilePath
        ,database :: FilePath
        ,repeat_ :: Int
        ,language :: Language
        ,scope :: String
        }
    | Test
        {deep :: Bool
        ,database :: FilePath
        ,language :: Language
        }
      deriving (Data,Typeable,Show)

defaultDatabaseLang :: Language -> IO FilePath
defaultDatabaseLang lang = do
    dir <- getAppUserDataDirectory "hoogle"
    return $ dir </> "default-" ++ lower (show lang) ++ "-" ++ showVersion (trimVersion 3 version) ++ ".hoo"

getCmdLine :: [String] -> IO CmdLine
getCmdLine args = do
    args <- withArgs args $ cmdArgsRun cmdLineMode

    -- fill in the default database
    args <- if database args /= "" then return args else do
        db <- defaultDatabaseLang $ language args; return args{database=db}

    -- fix up people using Hoogle 4 instructions
    args <- case args of
        Generate{..} | "all" `elem` include -> do
            putStrLn "Warning: 'all' argument is no longer required, and has been ignored."
            return $ args{include = delete "all" include}
        _ -> return args

    return args


defaultGenerate :: CmdLine
defaultGenerate = generate{language=Haskell}


cmdLineMode = cmdArgsMode $ modes [search_ &= auto,generate,server,replay,test]
    &= verbosity &= program "hoogle"
    &= summary ("Hoogle " ++ showVersion version ++ ", http://hoogle.haskell.org/")

search_ = Search
    {color = def &= name "colour" &= help "Use colored output (requires ANSI terminal)"
    ,link = def &= help "Give URL's for each result"
    ,numbers = def &= help "Give counter for each result"
    ,info = def &= help "Give extended information about the first result"
    ,database = def &= typFile &= help "Name of database to use (use .hoo extension)"
    ,count = 10 &= name "n" &= help "Maximum number of results to return"
    ,query = def &= args &= typ "QUERY"
    ,repeat_ = 1 &= help "Number of times to repeat (for benchmarking)"
    ,language = enum [x &= explicit &= name (lower $ show x) &= help ("Work with " ++ show x) | x <- [minBound..maxBound]] &= groupname "Language"
    ,compare_ = def &= help "Type signatures to compare against"
    } &= help "Perform a search"

generate = Generate
    {download = def &= help "Download all files from the web"
    ,insecure = def &= help "Allow insecure HTTPS connections"
    ,include = def &= args &= typ "PACKAGE"
    ,local_ = def &= opt "" &= help "Index local packages and link to local haddock docs"
    ,count = 0 &= name "n" &= help "Maximum number of packages to index (defaults to all)"
    ,haddock = def &= help "Use local haddocks"
    ,debug = def &= help "Generate debug information"
    } &= help "Generate Hoogle databases"

server = Server
    {port = 8080 &= typ "INT" &= help "Port number"
    ,cdn = "" &= typ "URL" &= help "URL prefix to use"
    ,logs = "" &= opt "log.txt" &= typFile &= help "File to log requests to (defaults to stdout)"
    ,local = False &= help "Allow following file:// links, restricts to 127.0.0.1  Set --host explicitely (including to '*' for any host) to override the localhost-only behaviour"
    ,haddock = def &= help "Serve local haddocks from a specified directory"
    ,scope = def &= help "Default scope to start with"
    ,links = def &= help "Display extra links"
    ,home = "http://hoogle.haskell.org" &= typ "URL" &= help "Set the URL linked to by the Hoogle logo."
    ,host = "" &= help "Set the host to bind on (e.g., an ip address; '!4' for ipv4-only; '!6' for ipv6-only; default: '*' for any host)."
    ,https = def &= help "Start an https server (use --cert and --key to specify paths to the .pem files)"
    ,cert = "cert.pem" &= typFile &= help "Path to the certificate pem file (when running an https server)"
    ,key = "key.pem" &= typFile &= help "Path to the key pem file (when running an https server)"
    ,datadir = def &= help "Override data directory paths"
    ,no_security_headers = False &= help "Don't send CSP security headers"
    } &= help "Start a Hoogle server"

replay = Replay
    {logs = "log.txt" &= args &= typ "FILE"
    } &= help "Replay a log file"

test = Test
    {deep = False &= help "Run extra long tests"
    } &= help "Run the test suite"
