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
        {web :: Bool                        -- ^ Are you operating from the web (via CGI)
        ,start :: Maybe Int
        ,count :: Maybe Int
        ,webmode :: Maybe String
        ,info :: Bool
        ,link :: Bool
        ,color :: Bool
        ,repeat_ :: Int
        ,databases :: [FilePath]
        ,queryChunks :: [String]
        
        ,queryParsed :: Either ParseError Query
        ,queryText :: String
        }
    | Test {testFiles :: [String], example :: Bool}
    | Server {port :: Int, databases :: [FilePath], resources :: FilePath, local_ :: Bool}
    | Dump {database :: String, section :: [String]}
    | Rank {srcfile :: FilePath}
    | Combine {srcfiles :: [FilePath], outfile :: String}
    | Convert {srcfile :: String, outfile :: String}
    | Data {datadir :: FilePath, threads :: Int, haddock :: Bool, redownload :: Bool, actions :: [String], local :: [String]}
      deriving (Data,Typeable,Show)

blankSearch = Search False Nothing Nothing Nothing False False False 1 [] [] (Left emptyParseError) ""

cmdLineMode = cmdArgsMode $ modes [search &= auto,test,server,dump,rank,combine,convert,dataa]
    &= verbosity &= program "hoogle"
    &= summary ("Hoogle v" ++ showVersion version ++ ", (C) Neil Mitchell 2004-2010\nhttp://haskell.org/hoogle")

search = Search
    {web = def &= help "Operate as a web tool"
    ,start = def
    ,count = def
    ,webmode = def
    ,queryChunks = def &= args
    ,info = def
    ,link = def
    ,color = def
    ,databases = ["."] &= typDir &= help "Which directories to find databases"
    ,repeat_ = 1
    ,queryParsed = Left emptyParseError &= ignore
    ,queryText = "" &= ignore
    }

test = Test
    {testFiles = def &= typFile &= args
    ,example = def &= help "Test the full examples"
    } &= help "Run tests over a list of files"

server = Server
    {port = 80 &= typ "INT" &= help "Port number"
    ,resources = ""
    ,local_ = def &= help "Rewrite file:/// links and serve them, can be a security hole"
    } &= help "Start a Hoogle server"

dump = Dump
    {database = def &= argPos 0 &= typ "DATABASE"
    ,section = def &= args &= typ "SECTION"
    } &= help "Dump sections of the database to stdout"

rank = Rank
    {srcfile = def &= argPos 0 &= typ "RANKFILE" &= opt ""
    } &= help "Generate ranking information"

combine = Combine
    {srcfiles = def &= args &= typ "DATABASE"
    ,outfile = "default.hoo" &= typFile &= help "Output file (defaults to default.hoo)"
    } &= help "Combine multiple inputs to produce one output"

convert = Convert
    {srcfile = def &= argPos 0 &= typ "TEXTBASE"
    ,outfile = def &= argPos 1 &= typ "DATABASE" &= opt ""
    } &= help "Convert a textbase to a database"

dataa = Data
    {datadir = def &= typDir &= help "Database directory"
    ,threads = def &= typ "INT" &= name "j" &= help "Number of threads to use"
    ,haddock = def &= help "Get haddock documentation directly from Hackage"
    ,redownload = def &= help "Always redownload files from the web"
    ,actions = def &= args &= typ "RULE"
    ,local = def &= opt "" &= typ "FILEPATH" &= help "Use local documentation where found"
    } &= help "Generate Hoogle databases"
      &= details ["Each argument should be the name of a database you want to generate"
                 ,"optionally followed by which files to combine. Common options:"
                 ,""
                 ,"  data default -- equialent to no arguments"
                 ,"  data all"
                 ]
