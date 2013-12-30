{-|
    Parse a query, that may have come from either a CGI variable
    or the command line arguments.

    Need to return the following pieces of information:

    * Was there a query, or was nothing entered

    * Are you wanting to operate in Web mode or Command Line mode. Adding a
    Web parameter to Command Line gives you Web mode.

    * Which flags were specified, and which were erroneous.
-}
module CmdLine.All(
    cmdLine, cmdLineWeb, CmdLine(..), isWebCmdLine,
    module CmdLine.Load
    ) where

import General.Base
import General.System
import System.FilePath
import CmdLine.Type
import CmdLine.Load
import General.Web
import System.Console.CmdArgs
import Hoogle
import Hoogle.Query.Type
import GHC.Conc(numCapabilities)
import Paths_hoogle
import Safe


---------------------------------------------------------------------
-- CMDLINE EXPANSION

cmdLineExpand :: CmdLine -> IO CmdLine
cmdLineExpand x@Search{} = do
    db <- expandDatabases $ databases x
    return x { queryText = s
               , queryParsed =
                   (\q -> q { exactSearch =
                                   if exact x
                                   then Just UnclassifiedItem
                                   else Nothing })
                   `fmap` parseQuery Haskell s
               , databases = db
               }
    where s = unwords $ queryChunks x


cmdLineExpand x@Server{} = do
    dat <- getDataDir
    db <- expandDatabases $ databases x
    let res = if null $ resources x then dat </> "resources" else resources x
    return x{databases=db, resources=res}

cmdLineExpand x@Test{} = do
    dat <- getDataDir
    let files1 = if null $ testFiles x then [dat </> "tests.txt"] else testFiles x
        files2 = [dat </> "examples.txt" | example x]
    return x{testFiles = files1 ++ files2}

cmdLineExpand x@Rank{} = do
    file <- if null $ srcfile x then fmap (</> "rank.txt") getDataDir else return $ srcfile x
    return x{srcfile=file}

cmdLineExpand x@Data{} = do
    dir <- if null $ datadir x then fmap (</> "databases") getDataDir else return $ datadir x
    let thrd = if threads x == 0 then numCapabilities else threads x
    loc <- if all null (local x) && not (null $ local x) then guessLocal else return $ local x
    return x{datadir=dir, threads=thrd, local=loc}

cmdLineExpand x@Convert{} =
  return x{haddock = haddock x || isJust (doc x),
    outfile = if null (outfile x) then replaceExtension (srcfile x) "hoo" else outfile x}

cmdLineExpand x = return x


expandDatabases x = do
    d <- getDataDir
    return $ x ++ [d </> "databases"]


guessLocal = do
    ghc <- findExecutable "ghc"
    home <- getHomeDirectory
    lib <- getLibDir
    let xs = [takeDirectory (takeDirectory lib) </> "doc" {- Windows, installed with Cabal -}  ] ++
             [takeDirectory (takeDirectory ghc) </> "doc/html/libraries" | Just ghc <- [ghc] {- Windows, installed by GHC -} ] ++
             [home </> ".cabal/share/doc" {- Linux -} ]
    filterM doesDirectoryExist xs


---------------------------------------------------------------------
-- QUERY CONVERSION

cmdLine :: IO CmdLine
cmdLine = do
    r <- cgiArgs
    case r of
        Just y -> cmdLineWeb y
        Nothing -> cmdLineArgs


cmdLineArgs :: IO CmdLine
cmdLineArgs = cmdLineExpand =<< cmdArgsRun cmdLineMode


cmdLineWeb :: [(String,String)] -> IO CmdLine
cmdLineWeb args = cmdLineExpand $ blankSearch
        {web=Just $ fromMaybe "web" $ ask ["mode"]
        ,start=askInt ["start"], count=askInt ["count"]
        ,exact=fromMaybe 0 (askInt ["exact"]) == 1
        ,queryChunks = mapMaybe ask [["prefix"],["q","hoogle"],["suffix"]]}
    where ask x = listToMaybe [b | (a,b) <- args, a `elem` x]
          askInt x = readMay =<< ask x
