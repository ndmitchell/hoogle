{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module CmdLine.Type(
    CmdLine(..), cmdLineMode, isWebCmdLine, blankSearch
    ) where

import System.Console.CmdArgs
import Paths_hoogle(version)
import Data.Version(showVersion)
import Hoogle


isWebCmdLine Search{web=True} = True
isWebCmdLine Server{} = True
isWebCmdLine _ = False


data CmdLine
    = Search
        {color :: Bool
        ,link :: Bool
        ,info :: Bool
        ,databases :: [FilePath]
        ,start :: Maybe Int
        ,count :: Maybe Int
        ,web :: Bool
        ,webmode :: Maybe String
        ,repeat_ :: Int
        ,queryChunks :: [String]
        
        ,queryParsed :: Either ParseError Query
        ,queryText :: String
        }
    | Data {redownload :: Bool, local :: [String], datadir :: FilePath, threads :: Int, actions :: [String]}
    | Server {port :: Int, local_ :: Bool, databases :: [FilePath], resources :: FilePath, nostdin :: Bool}
    | Combine {srcfiles :: [FilePath], outfile :: String}
    | Convert {srcfile :: String, outfile :: String}
    | Test {testFiles :: [String], example :: Bool}
    | Dump {database :: String, section :: [String]}
    | Rank {srcfile :: FilePath}
      deriving (Data,Typeable,Show)

emptyParseError = ParseError 0 0 "" $ Str ""
blankSearch = Search False False False [] Nothing Nothing False Nothing 1 [] (Left emptyParseError) ""

cmdLineMode = cmdArgsMode $ modes [search_ &= auto,dataa,server,combine,convert,test,dump,rank]
    &= verbosity &= program "hoogle"
    &= summary ("Hoogle v" ++ showVersion version ++ ", (C) Neil Mitchell 2004-2011\nhttp://haskell.org/hoogle")

search_ = Search
    {web = def &= help "Operate as a web tool"
    ,start = def &= help "Start displaying results from this point on (1 based)"
    ,count = def &= name "n" &= help "Maximum number of results to return"
    ,webmode = def &= typ "MODE" &= help "Specify a mode when running as a web tool"
    ,queryChunks = def &= args &= typ "QUERY"
    ,info = def &= help "Give extended information about the first result"
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
    ,nostdin = def &= help "Don't read from stdin"
    ,local_ = def &= help "Rewrite and serve file: links (potential security hole)"
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
    {srcfile = def &= argPos 0 &= typ "INPUT"
    ,outfile = def &= argPos 1 &= typ "DATABASE" &= opt ""
    } &= help "Convert an input file to a database"

dataa = Data
    {datadir = def &= typDir &= help "Database directory"
    ,redownload = def &= help "Redownload all files from the web"
    ,threads = def &= typ "INT" &= name "j" &= help "Number of threads to use" &= ignore -- ignore until it works
    ,actions = def &= args &= typ "RULE"
    ,local = def &= opt "" &= typ "FILEPATH" &= help "Use local documentation if available"
    } &= help "Generate Hoogle databases"
      &= details ["Each argument should be the name of a database you want to generate"
                 ,"optionally followed by which files to combine. Common options:"
                 ,""
                 ,"  data default -- equialent to no arguments"
                 ,"  data all"
                 ]
