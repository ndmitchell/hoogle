{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Generate(actionGenerate) where

import Data.List.Extra
import System.FilePath
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import Control.Exception.Extra
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad.Extra
import Numeric.Extra
import System.Console.CmdArgs.Verbosity
import Prelude

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Input.Cabal
import Input.Haddock
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

data Timing = Timing (Maybe (IORef [(String, Double)]))

withTiming :: Maybe FilePath -> (Timing -> IO a) -> IO a
withTiming file f = do
    offset <- offsetTime
    ref <- newIORef []
    res <- f $ Timing $ if isJust file then Just ref else Nothing
    end <- offset
    whenJust file $ \file -> do
        ref <- readIORef ref
        -- Expecting unrecorded of ~2s
        -- Most of that comes from the pipeline - we get occasional 0.01 between items as one flushes
        -- Then at the end there is ~0.5 while the final item flushes
        ref <- return $ reverse $ sortOn snd $ ("Unrecorded",end - sum (map snd ref)) : ref
        writeFile file $ unlines [showDP 2 b ++ "\t" ++ a | (a,b) <- ("Total",end) : ref]
    putStrLn $ "Took " ++ showDuration end
    return res


timed :: MonadIO m => Timing -> String -> m a -> m a
timed (Timing ref) msg act = do
    liftIO $ putStr (msg ++ "... ") >> hFlush stdout
    time <- liftIO offsetTime
    res <- act
    time <- liftIO time
    stats <- liftIO getGCStatsEnabled
    s <- if not stats then return "" else do GCStats{..} <- liftIO getGCStats; return $ " (" ++ show peakMegabytesAllocated ++ "Mb)"
    liftIO $ putStrLn $ showDuration time ++ s
    case ref of -- don't use whenJust, induces Appliative pre-AMP
        Nothing -> return ()
        Just ref -> liftIO $ modifyIORef ref ((msg,time):)
    return res


actionGenerate :: CmdLine -> IO ()
actionGenerate g@Generate{..} = withTiming (if debug then Just $ replaceExtension database "timing" else Nothing) $ \timing -> do
    putStrLn "Starting generate"
    createDirectoryIfMissing True $ takeDirectory database
    downloadInputs (timed timing) insecure download $ takeDirectory database
    gcStats <- getGCStatsEnabled

    -- fix up people using Hoogle 4 instructions
    args <- if "all" `notElem` include then return include else do
        putStrLn $ "Warning: 'all' argument is no longer required, and has been ignored."
        return $ delete "all" include

    -- peakMegabytesAllocated = 2
    let input x = takeDirectory database </> "input-" ++ x

    setStackage <- setStackage $ input "stackage.txt"
    setPlatform <- setPlatform $ input "platform.txt"
    setGHC <- setGHC $ input "platform.txt"
    let want = if args /= [] then Set.fromList args else
            Set.insert "ghc" $ Set.unions [setStackage, setPlatform, setGHC]
    -- peakMegabytesAllocated = 2

    (cblErrs,cbl) <- timed timing "Reading Cabal" $ parseCabalTarball $ input "cabal.tar.gz"
    let packageTags pkg =
            [("set","included-with-ghc") | pkg `Set.member` setGHC] ++
            [("set","haskell-platform") | pkg `Set.member` setPlatform] ++
            [("set","stackage") | pkg `Set.member` setStackage] ++
            maybe [] (map (both T.unpack) . cabalTags) (Map.lookup pkg cbl)
    -- peakMegabytesAllocated = 21, currentBytesUsed = 6.5Mb

    (stats, _) <- storeWriteFile database $ \store -> do
        xs <- withBinaryFile (database `replaceExtension` "warn") WriteMode $ \warnings -> do
            hSetEncoding warnings utf8
            hPutStr warnings $ unlines cblErrs
            nCblErrs <- evaluate $ length cblErrs

            itemWarn <- newIORef 0
            let warning msg = do modifyIORef itemWarn succ; hPutStrLn warnings msg

            let consume :: Conduit (Int, (String, LStr)) IO (Maybe Target, Item)
                consume = awaitForever $ \(i, (pkg, body)) -> do
                    timed timing ("[" ++ show i ++ "/" ++ show (Set.size want) ++ "] " ++ pkg) $
                        parseHoogle warning pkg body

            writeItems store $ \items -> do
                let packages = [ fakePackage name $ "Not in Stackage, so not searched.\n" ++ T.unpack cabalSynopsis
                               | (name,Cabal{..}) <- Map.toList cbl, name `Set.notMember` want]

                (seen, xs) <- runConduit $
                    (do sourceList =<< liftIO (tarballReadFiles $ input "hoogle.tar.gz")
                        src <- liftIO $ strReadFile $ input "ghc.txt"
                        Just (_, rest) <- return $ strSplitInfix (strPack "-- |") src
                        let url = "@url http://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/"
                        yield ("ghc.txt", lstrFromChunks [strPack $ url ++ "\n-- |", rest])) =$=
                    mapC (first takeBaseName) =$=
                    filterC (flip Set.member want . fst) =$=
                        ((fmap Set.fromList $ mapC fst =$= sinkList) |$|
                        (((zipFromC 1 =$= consume) >> when (null args) (sourceList packages))
                            =$= pipelineC 10 (items =$= sinkList)))

                let missing = [x | x <- Set.toList $ want `Set.difference` seen
                                 , fmap cabalLibrary (Map.lookup x cbl) /= Just False]
                whenNormal $ when (missing /= []) $ do
                    putStrLn $ ("Packages not found: " ++) $ unwords $ sortOn lower missing
                when (Set.null seen) $
                    exitFail "No packages were found, aborting (use no arguments to index all of Stackage)"

                itemWarn <- readIORef itemWarn
                when (itemWarn > 0) $
                    putStrLn $ "Found " ++ show itemWarn ++ " warnings when processing items"
                return xs

        itemsMb <- if not gcStats then return 0 else do performGC; GCStats{..} <- getGCStats; return $ currentBytesUsed `div` (1024*1024)
        xs <- timed timing "Reodering items" $ reorderItems (\s -> maybe 1 (negate . cabalPopularity) $ Map.lookup s cbl) xs
        timed timing "Writing tags" $ writeTags store (`Set.member` want) packageTags xs
        timed timing "Writing names" $ writeNames store xs
        timed timing "Writing types" $ writeTypes store (if debug then Just $ dropExtension database else Nothing) xs

        when gcStats $ do
            stats@GCStats{..} <- getGCStats
            x <- getVerbosity
            when (x >= Loud) $
                print stats
            when (x >= Normal) $ do
                putStrLn $ "Peak of " ++ show peakMegabytesAllocated ++ "Mb, " ++ show itemsMb ++ "Mb for items"

    when debug $
        writeFile (database `replaceExtension` "store") $ unlines stats
