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
    | Server
    | Dump {database :: String, section :: [String]}
    | Rank FilePath
    | Combine FilePath
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

dump = Dump
    {database = def &= argPos 0 &= typ "DATABASE"
    ,section = def &= args &= typ "SECTION"
    } &= help "Dump sections of the database to stdout"

rank = Rank def

combine = Combine def

convert = Convert
    {srcfile = def &= argPos 0 &= typ "TEXTBASE"
    ,outfile = def &= argPos 1 &= typ "DATABASE"
    } &= help "Convert a textbase to a database"


{-
data CmdFlag = Version           -- ^ Version information
             | Web               -- ^ Operate as a CGI process
             | Help              -- ^ Help text
             | Test              -- ^ Run the regression tests
             | Color Bool        -- ^ Colors on the console
             | Start Int         -- ^ First result to show
             | Count Int         -- ^ Number of results to show
             | Convert FilePath  -- ^ Convert a database
             | Output FilePath   -- ^ Output file
             | Dump String       -- ^ Dump a database to a file (optional section)
             | DataFile FilePath -- ^ Database location
             | Verbose           -- ^ Display verbose information
             | Info              -- ^ Display as much information as you can
             | Debug             -- ^ Do debugging activities
             | Include FilePath  -- ^ Include directory
             | TestFile FilePath -- ^ Run tests in a file
             | Rank FilePath     -- ^ Generate rankings
             | Combine FilePath  -- ^ Merge a set of databases
             | Mode String       -- ^ Web modes
             | Server            -- ^ Server

[f (ArgNone Version) ["version","ver"] [PCmdLine] "Print out version information"
,f (ArgNone Help) ["?","help","h"] [PCmdLine] "Show help message"
,f (ArgNone Web) ["w","web"] [PCmdLine] "Run as though it was a CGI script"
,f (ArgBool Color) ["c","color","col","colour"] [PCmdLine] "Show color output (default=false)"
,f (ArgPos  Start) ["s","start"] [PCmdLine,PWebArgs] "First result to show (default=1)"
,f (ArgPos  Count) ["n","count","length","len"] [PCmdLine,PWebArgs] "Number of results to show (default=all)"
,f (ArgNone Test) ["test"] [PCmdLine] "Run the regression tests"
,f (ArgFileIn Convert ["txt"]) ["convert"] [PCmdLine,PMultiple] "Convert a database"
,f (ArgFileOut Output) ["output"] [PCmdLine] "Output file for convert"
,f (ArgStr  Dump) ["dump"] [PCmdLine] "Dump a database for debugging"
,f (ArgFileIn DataFile ["hoo"]) ["d","data"] [PCmdLine,PMultiple] "Database file"
,f (ArgNone Verbose) ["v","verbose"] [PCmdLine] "Display verbose information"
,f (ArgNone Info) ["info"] [PCmdLine] "Display full information on an entry"
,f (ArgNone Debug) ["debug"] [PCmdLine] "Debugging only"
,f (ArgDir Include) ["i","include"] [PCmdLine,PMultiple] "Include directories"
,f (ArgFileIn TestFile ["txt"]) ["testfile"] [PCmdLine,PMultiple] "Run tests from a file"
,f (ArgFileIn Rank ["txt"]) ["rank"] [PCmdLine,PMultiple] "Generate ranking scores"
,f (ArgFileIn Combine ["hoo"]) ["combine"] [PCmdLine,PMultiple] "Combine multiple databases"
,f (ArgStr Mode) ["mode"] [PCmdLine,PWebArgs] "Web mode"
,f (ArgNone Server) ["server"] [PCmdLine] "Start a server"
]
-}
