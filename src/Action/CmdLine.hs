{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse #-}

module Action.CmdLine(CmdLine(..), getCmdLine) where

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import Data.Version
import Paths_hoogle(version)


data CmdLine
    = Search
        {color :: Maybe Bool
        ,link :: Bool
        ,info :: Bool
        ,database :: FilePath
        ,count :: Int
        ,query :: [String]
        ,repeat_ :: Int
        }
    | Generate
        {hackage :: String
        ,download :: Maybe Bool
        ,database :: FilePath
        ,include :: [String]
        ,debug :: Bool
        }
    | Server
        {port :: Int
        ,database :: FilePath
        ,cdn :: String
        ,logs :: FilePath
        }
    | Replay
        {logs :: FilePath
        ,database :: FilePath
        ,repeat_ :: Int
        }
    | Test
        {deep :: Bool
        ,database :: FilePath
        }
      deriving (Data,Typeable,Show)

getCmdLine :: IO CmdLine
getCmdLine = do
    args <- cmdArgsRun cmdLineMode
    if database args /= "" then return args else do
        dir <- getAppUserDataDirectory "hoogle"
        return $ args{database=dir </> "default-" ++ showVersion version ++ ".hoo"}

cmdLineMode = cmdArgsMode $ modes [search_ &= auto,generate,server,replay,test]
    &= verbosity &= program "hoogle"
    &= summary ("Hoogle " ++ showVersion version ++ ", http://hoogle.haskell.org/")

search_ = Search
    {color = def &= name "colour" &= help "Use colored output (requires ANSI terminal)"
    ,link = def &= help "Give URL's for each result"
    ,info = def &= help "Give extended information about the first result"
    ,database = def &= typFile &= help "Name of database to use (use .hoo extension)"
    ,count = 10 &= name "n" &= help "Maximum number of results to return"
    ,query = def &= args &= typ "QUERY"
    ,repeat_ = 1 &= help "Number of times to repeat (for benchmarking)"
    } &= help "Perform a search"

generate = Generate
    {hackage = "https://hackage.haskell.org/" &= typ "URL" &= help "Hackage instance to target"
    ,download = def &= help "Download all files from the web"
    ,include = def &= args &= typ "PACKAGE"
    ,debug = def &= help "Generate debug information"
    } &= help "Generate Hoogle databases"

server = Server
    {port = 80 &= typ "INT" &= help "Port number"
    ,cdn = "" &= typ "URL" &= help "URL prefix to use"
    ,logs = "" &= opt "log.txt" &= typFile &= help "File to log requests to (defaults to stdout)"
    } &= help "Start a Hoogle server"

replay = Replay
    {logs = "log.txt" &= args &= typ "FILE"
    } &= help "Replay a log file"

test = Test
    {deep = False &= help "Run extra long tests"
    } &= help "Run the test suite"
