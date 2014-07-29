{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse #-}

module CmdLine.Type(
    CmdLine(..), cmdLineMode, isWebCmdLine, blankSearch
    ) where

import System.Console.CmdArgs
import Paths_hoogle(version)
import Data.Version(showVersion)
import Hoogle


isWebCmdLine Search{web=Just _} = True
isWebCmdLine Server{} = True
isWebCmdLine _ = False


data CmdLine
    = Search
        {color :: Bool
        ,link :: Bool
        ,info :: Bool
        ,exact :: Bool
        ,databases :: [FilePath]
        ,start :: Maybe Int
        ,count :: Maybe Int
        ,web :: Maybe String
        ,repeat_ :: Int
        ,queryChunks :: [String]
        ,queryParsed :: Either ParseError Query
        ,queryText :: String
        }
    | Data {
          hackage    :: String
        , redownload :: Bool
        , rebuild :: Bool
        , local :: [String]
        , datadir :: FilePath
        , threads :: Int
        , actions :: [String]
        , nodownload :: Bool
        }
    | Server {port :: Int, local_ :: Bool, databases :: [FilePath], resources :: FilePath, dynamic :: Bool, template :: [FilePath]}
    | Combine {srcfiles :: [FilePath], outfile :: String}
    | Convert {
          hackage :: String
        , srcfile :: String
        , outfile :: String
        , doc :: Maybe String
        , merge :: [String]
        , haddock :: Bool}
    | Log {logfiles :: [FilePath]}
    | Test {testFiles :: [String], example :: Bool}
    | Dump {database :: String, section :: [String]}
    | Rank {srcfile :: FilePath}
      deriving (Data,Typeable,Show)

emptyParseError = ParseError 0 0 "" $ Str ""
blankSearch = Search False False False False [] Nothing Nothing Nothing 1 [] (Left emptyParseError) ""

cmdLineMode = cmdArgsMode $ modes [search_ &= auto,data_,server,combine,convert,test,dump,rank,log_]
    &= verbosity &= program "hoogle"
    &= summary ("Hoogle v" ++ showVersion version ++ ", (C) Neil Mitchell 2004-2012\nhttp://haskell.org/hoogle")

search_ = Search
    {web = def &= typ "MODE" &= opt "web" &= help "Operate as a web tool"
    ,start = def &= help "Start displaying results from this point on (1 based)"
    ,count = def &= name "n" &= help "Maximum number of results to return"
    ,queryChunks = def &= args &= typ "QUERY"
    ,info = def &= help "Give extended information about the first result"
    ,exact = def &= help "Match names exactly when searching"
    ,link = def &= help "Give URL's for each result"
    ,color = def &= name "colour" &= help "Use colored output (requires ANSI terminal)"
    ,databases = ["."] &= typDir &= help "Directories to search for databases"
    ,repeat_ = 1 &= help "Run the search multiple times (for benchmarking)"
    ,queryParsed = Left emptyParseError &= ignore
    ,queryText = "" &= ignore
    } &= help "Perform a search"

test = Test
    {testFiles = def &= typFile &= args
    ,example = def &= help "Test the full examples"
    } &= help "Run tests"

server = Server
    {port = 80 &= typ "INT" &= help "Port number"
    ,resources = "" &= typDir &= help "Directory to use for resources (images, CSS etc)"
    ,local_ = def &= help "Rewrite and serve file: links (potential security hole)"
    ,dynamic = def &= name "x" &= help "Allow resource files to change during execution"
    ,template = def &= typFile &= help "Template files to use instead of default definitions"
    } &= help "Start a Hoogle server"

dump = Dump
    {database = def &= argPos 0 &= typ "DATABASE"
    ,section = def &= args &= typ "SECTION"
    } &= help "Dump sections of a database to stdout"

rank = Rank
    {srcfile = def &= argPos 0 &= typ "RANKFILE" &= opt ""
    } &= help "Generate ranking information"

combine = Combine
    {srcfiles = def &= args &= typ "DATABASE"
    ,outfile = "default.hoo" &= typFile &= help "Output file (defaults to default.hoo)"
    } &= help "Combine multiple databases into one"

convert = Convert
    {hackage = "http://hackage.haskell.org/" &= typ "URL" &= help "Hackage instance to target"
    ,srcfile = def &= argPos 0 &= typ "INPUT"
    ,outfile = def &= argPos 1 &= typ "DATABASE" &= opt ""
    ,doc = def &= typDir &= help "Path to the root of local or Hackage documentation for the package (implies --haddock)"
    ,merge = def &= typ "DATABASE" &= help "Merge other databases"
    ,haddock = def &= help "Apply haddock-specific hacks"
    } &= help "Convert an input file to a database"

data_ = Data
    {datadir = def &= typDir &= help "Database directory"
    ,hackage    = "http://hackage.haskell.org/" &= typ "URL" &= help "Hackage instance to target"
    ,redownload = def &= help "Redownload all files from the web"
    ,rebuild = def &= help "Rebuild everything"
    ,threads = 1 &= typ "INT" &= name "j" &= help "Number of threads to use"
    ,actions = def &= args &= typ "RULE"
    ,local = def &= opt "" &= typ "FILEPATH" &= help "Use local documentation if available"
    ,nodownload = def &= explicit &= name "no-download" &= help "Abort if any of the needed source files are missing, instead of downloading them"
    } &= help "Generate Hoogle databases"
      &= details ["Each argument should be the name of a database you want to generate"
                 ,"optionally followed by which files to combine. Common options:"
                 ,""
                 ,"  data default -- equivalent to no arguments"
                 ,"  data all"
                 ]

log_ = Log
    {logfiles = def &= args &= typ "LOGFILE"
    } &= help "Analyse log files"
