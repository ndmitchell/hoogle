{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Generate(actionGenerate) where

import Data.List.Extra
import System.FilePath
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import Control.Exception.Extra
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad.Extra
import System.Console.CmdArgs.Verbosity
import Prelude

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Input.Cabal
import Input.Hoogle
import Input.Download
import Input.Reorder
import Input.Set
import Input.Item
import General.Util
import General.Store
import General.Str
import System.Mem
import System.IO
import GHC.Stats
import Action.CmdLine
import General.Conduit

{-


data GenList
    = GenList_Package String -- a literally named package
    | GenList_GhcPkg String -- command to run, or "" for @ghc-pkg list@
    | GenList_Stackage String -- URL of stackage file, defaults to @http://www.stackage.org/lts/cabal.config@
    | GenList_Dependencies String -- dependencies in a named .cabal file
    | GenList_Sort String -- URL of file to sort by, defaults to @http://packdeps.haskellers.com/reverse@

data GenTags
    = GenTags_GhcPkg String -- command to run, or "" for @ghc-pkg dump@
    | GenTags_Diff FilePath -- a diff to apply to previous metadata
    | GenTags_Tarball String -- tarball of Cabal files, defaults to http://hackage.haskell.org/packages/index.tar.gz
    | GetTags_Cabal FilePath -- tarball to get tag information from

data GenData
    = GenData_File FilePath -- a file containing package data
    | GenData_Tarball String -- URL where a tarball of data files resides


* `hoogle generate` - generate for all things in Stackage based on Hackage information.
* `hoogle generate --source=file1.txt --source=local --source=stackage --source=hackage --source=tarball.tar.gz`

Which files you want to index. Currently the list on stackage, could be those locally installed, those in a .cabal file etc. A `--list` flag, defaults to `stackage=url`. Can also be `ghc-pkg`, `ghc-pkg=user` `ghc-pkg=global`. `name=p1`.

Extra metadata you want to apply. Could be a file. `+shake author:Neil-Mitchell`, `-shake author:Neil-Mitchel`. Can be sucked out of .cabal files. A `--tags` flag, defaults to `tarball=url` and `diff=renamings.txt`.

Where the haddock files are. Defaults to `tarball=hackage-url`. Can also be `file=p1.txt`. Use `--data` flag.

Defaults to: `hoogle generate --list=ghc-pkg --list=constrain=stackage-url`.

Three pieces of data:

* Which packages to index, in order.
* Metadata.


generate :: Maybe Int -> [GenList] -> [GenTags] -> [GenData] -> IO ()
-- how often to redownload, where to put the files



generate :: FilePath -> [(String, [(String, String)])] -> [(String, LBS.ByteString)] -> IO ()
generate output metadata  = undefined
-}


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

actionGenerate :: CmdLine -> IO ()
actionGenerate Generate{..} = do
    putStrLn "Starting generate"
    createDirectoryIfMissing True $ takeDirectory database
    downloadInputs $ takeDirectory database
    (n,_) <- duration $ generate database debug include
    putStrLn $ "Took " ++ showDuration n


generate :: FilePath -> Bool -> [String] -> IO ()
generate database debug args = do
    -- fix up people using Hoogle 4 instructions
    args <- if "all" `notElem` args then return args else do
        putStrLn $ "Warning: 'all' argument is no longer required, and has been ignored."
        return $ delete "all" args

    -- peakMegabytesAllocated = 2
    let input x = takeDirectory database </> "input-" ++ x

    setStackage <- setStackage $ input "stackage.txt"
    setPlatform <- setPlatform $ input "platform.txt"
    setGHC <- setGHC $ input "platform.txt"
    let want = if args /= [] then Set.fromList args else Set.unions [setStackage, setPlatform, setGHC]
    -- peakMegabytesAllocated = 2

    (cblErrs,cbl) <- timed "Reading Cabal" $ parseCabalTarball $ input "cabal.tar.gz"
    let packageTags pkg =
            [("set","included-with-ghc") | pkg `Set.member` setGHC] ++
            [("set","haskell-platform") | pkg `Set.member` setPlatform] ++
            [("set","stackage") | pkg `Set.member` setStackage] ++
            maybe [] (map (both T.unpack) . cabalTags) (Map.lookup pkg cbl)
    -- peakMegabytesAllocated = 21, currentBytesUsed = 6.5Mb

    storeWriteFile database $ \store -> do
        xs <- withBinaryFile (database `replaceExtension` "warn") WriteMode $ \warnings -> do
            hSetEncoding warnings utf8
            hPutStr warnings $ unlines cblErrs
            nCblErrs <- evaluate $ length cblErrs

            itemWarn <- newIORef 0
            let warning msg = do modifyIORef itemWarn succ; hPutStrLn warnings msg

            let consume :: Conduit (Int, (String, LStr)) IO (Target, Item)
                consume = awaitForever $ \(i, (pkg, body)) -> do
                    timed ("[" ++ show i ++ "/" ++ show (Set.size want) ++ "] " ++ pkg) $
                        parseHoogle warning pkg body

            writeItems store $ \items -> do
                let packages = [ (Target ("https://hackage.haskell.org/package/" ++ name) Nothing Nothing "package" (renderItem $ IPackage name) ("Not in Stackage, so not searched.\n" ++ T.unpack cabalSynopsis), IPackage name)
                               | (name,Cabal{..}) <- Map.toList cbl, name `Set.notMember` want]

                (seen, xs) <- runConduit $
                    (sourceList =<< liftIO (tarballReadFiles $ input "hoogle.tar.gz")) =$=
                    mapC (first takeBaseName) =$=
                    filterC (flip Set.member want . fst) =$=
                        ((fmap Set.fromList $ mapC fst =$= sinkList) |$|
                        (((zipFromC 1 =$= consume) >> when (null args) (sourceList packages))
                            =$= pipelineC 10 (items =$= sinkList)))

                putStrLn $ "Packages not found: " ++ unwords (Set.toList $ want `Set.difference` seen)
                when (Set.null seen) $
                    exitFail "No packages were found, aborting (use no arguments to index all of Stackage)"

                itemWarn <- readIORef itemWarn
                when (itemWarn > 0) $
                    putStrLn $ "Found " ++ show itemWarn ++ " warnings when processing items"
                return xs


        xs <- timed "Reodering items" $ reorderItems (\s -> maybe 1 (negate . cabalPopularity) $ Map.lookup s cbl) xs
        timed "Writing tags" $ writeTags store (`Set.member` want) packageTags xs
        timed "Writing names" $ writeNames store xs
        timed "Writing types" $ writeTypes store (if debug then Just $ dropExtension database else Nothing) xs

        whenM getGCStatsEnabled $ do
            performGC
            stats@GCStats{..} <- getGCStats
            x <- getVerbosity
            if x >= Loud then
                print stats
             else if x >= Normal then
                putStrLn $ "Required " ++ show peakMegabytesAllocated ++ "Mb, " ++ show (currentBytesUsed `div` 1024) ++ "Mb currently used"
             else
                return ()

            void $ evaluate xs
