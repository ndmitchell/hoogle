{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse #-}

module Action.CmdLine(CmdLine(..), getCmdLine) where

import System.Console.CmdArgs
import Paths_hoogle(version)
import Data.Version(showVersion)


data CmdLine
    = Search
        {color :: Maybe Bool
        ,link :: Bool
        ,info :: Bool
        ,database :: FilePath
        ,count :: Maybe Int
        ,query :: [String]
        }
    | Generate
        {hackage :: String
        ,download :: Maybe Bool
        ,database :: FilePath
        ,include :: [String]
        }
    | Server
        {port :: Int
        ,database :: FilePath
        ,datadir :: FilePath
        ,cdn :: String
        ,logs :: FilePath
        }
    | Replay
        {logs :: FilePath
        }
    | Test
      deriving (Data,Typeable,Show)

getCmdLine :: IO CmdLine
getCmdLine = cmdArgsRun cmdLineMode

cmdLineMode = cmdArgsMode $ modes [search_ &= auto,generate,server,replay,test]
    &= verbosity &= program "hoogle"
    &= summary ("Hoogle " ++ showVersion version ++ ", http://hoogle.haskell.org/")

search_ = Search
    {color = def &= name "colour" &= help "Use colored output (requires ANSI terminal)"
    ,link = def &= help "Give URL's for each result"
    ,info = def &= help "Give extended information about the first result"
    ,database = def &= typFile &= help "Location of database to use"
    ,count = def &= name "n" &= help "Maximum number of results to return"
    ,query = def &= args &= typ "QUERY"
    } &= help "Perform a search"

generate = Generate
    {hackage = "https://hackage.haskell.org/" &= typ "URL" &= help "Hackage instance to target"
    ,download = def &= help "Download all files from the web"
    ,include = def &= args &= typ "PACKAGE"
    } &= help "Generate Hoogle databases"

server = Server
    {port = 80 &= typ "INT" &= help "Port number"
    ,datadir = "" &= typDir &= help "Directory to use for resources (images, CSS etc)"
    ,cdn = "" &= typ "URL" &= help "URL prefix to use"
    ,logs = "" &= opt "log.txt" &= typFile &= help "File to log requests to (defaults to stdout)"
    } &= help "Start a Hoogle server"

replay = Replay
    {logs = "log.txt" &= args &= typ "FILE"
    } &= help "Replay a log file"

test = Test{} &= help "Run the test suite"

