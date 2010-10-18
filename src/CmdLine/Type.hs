{-# LANGUAGE DeriveDataTypeable #-}

module CmdLine.Type(
    CmdLine(..), cmdLineMode, isWebCmdLine, blankSearch
    ) where

import System.Console.CmdArgs
import Paths_hoogle(version)
import Data.Version(showVersion)
import Hoogle.Query.All(Query)
import Data.Monoid


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
        ,color :: Bool
        ,databaseDir :: [FilePath]
        ,queryChunks :: [String]
        
        ,queryParsed :: Either (Int,String) Query
        ,queryText :: String
        }
    | Test {testFiles :: [String]}
    | Server {port :: Int}
    | Dump {database :: String, section :: [String]}
    | Rank {srcfile :: FilePath}
    | Combine {srcfiles :: FilePath, outfile :: String}
    | Convert {srcfile :: String, outfile :: String}
      deriving (Data,Typeable,Show)

blankSearch = Search False Nothing Nothing Nothing False False [] [] (Right mempty) ""

cmdLineMode = cmdArgsMode $ modes [search &= auto,test,server,dump,rank,combine,convert]
    &= verbosity
    &= summary ("Hoogle v" ++ showVersion version ++ ", (C) Neil Mitchell 2004-2010\nhttp://haskell.org/hoogle")

search = Search
    {web = def &= help "Operate as a web tool"
    ,start = def
    ,count = def
    ,webmode = def
    ,queryChunks = def &= args
    ,info = def
    ,color = def
    ,databaseDir = def
    ,queryParsed = Right mempty &= ignore
    ,queryText = "" &= ignore
    }

test = Test {testFiles = def &= args}

server = Server
    {port = 80 &= typ "INT" &= help "Port number"
    } &= help "Start a Hoogle server"

dump = Dump
    {database = def &= argPos 0 &= typ "DATABASE"
    ,section = def &= args &= typ "SECTION"
    } &= help "Dump sections of the database to stdout"

rank = Rank
    {srcfile = def &= argPos 0 &= typ "RANKFILE" &= opt "rank.txt"
    } &= help "Generate ranking information"

combine = Combine
    {srcfiles = def &= args &= typ "INPUT"
    ,outfile = def &= argPos 0 &= typ "OUTPUT"
    } &= help "Combine multiple inputs to produce one output"

convert = Convert
    {srcfile = def &= argPos 0 &= typ "TEXTBASE"
    ,outfile = def &= argPos 1 &= typ "DATABASE" &= opt ""
    } &= help "Convert a textbase to a database"
