{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Generate(actionGenerate) where

import Data.List.Extra
import System.FilePath
import System.Directory.Extra
import Data.Tuple.Extra
import Control.Exception.Extra
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad.Extra
import Control.Applicative
import Data.Monoid
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
import Input.Settings
import Input.Item
import General.Util
import General.Store
import General.Timing
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

type Download = String -> URL -> IO FilePath

readHaskellOnline :: Timing -> Settings -> Download -> IO (Map.Map String Package, Set.Set String, Source IO (String, URL, LStr))
readHaskellOnline timing settings download = do
    stackage <- download "haskell-stackage.txt" "https://www.stackage.org/lts/cabal.config"
    platform <- download "haskell-platform.txt" "https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/src/Releases2015.hs"
    ghcapi   <- download "haskell-ghcapi.txt" $ "https://downloads.haskell.org/~ghc/" ++ ghcApiVersion ++ "/docs/html/libraries/ghc-" ++ ghcApiVersion ++ "/ghc.txt"
    cabals   <- download "haskell-cabal.tar.gz" "https://hackage.haskell.org/packages/index.tar.gz"
    hoogles  <- download "haskell-hoogle.tar.gz" "https://hackage.haskell.org/packages/hoogle.tar.gz"

    -- peakMegabytesAllocated = 2
    setStackage <- setStackage stackage
    setPlatform <- setPlatform platform
    setGHC <- setGHC platform

    cbl <- timed timing "Reading Cabal" $ parseCabalTarball settings cabals
    let want = Set.insert "ghc" $ Set.unions [setStackage, setPlatform, setGHC]
    cbl <- return $ flip Map.mapWithKey cbl $ \name p ->
        p{packageTags =
            [(T.pack "set",T.pack "included-with-ghc") | name `Set.member` setGHC] ++
            [(T.pack "set",T.pack "haskell-platform") | name `Set.member` setPlatform] ++
            [(T.pack "set",T.pack "stackage") | name `Set.member` setStackage] ++
            packageTags p}

    let source = do
            tar <- liftIO $ tarballReadFiles hoogles
            forM_ tar $ \(takeBaseName -> name, src) ->
                yield (name, hackagePackageURL name, src)
            src <- liftIO $ strReadFile ghcapi
            let url = "https://downloads.haskell.org/~ghc/" ++ ghcApiVersion ++ "/docs/html/libraries/ghc-" ++ ghcApiVersion ++ "/"
            yield ("ghc", url, lstrFromChunks [src])
    return (cbl, want, source)


readHaskellDirs :: Timing -> [FilePath] -> IO (Map.Map String Package, Set.Set String, Source IO (String, URL, LStr))
readHaskellDirs timing dirs = do
    packages <- map (takeBaseName &&& id) . filter ((==) ".txt" . takeExtension) <$> concatMapM listFilesRecursive dirs
    let source = forM_ packages $ \(name, file) -> do
            src <- liftIO $ strReadFile file
            dir <- liftIO $ canonicalizePath $ takeDirectory file
            let url = "file://" ++ ['/' | not $ "/" `isPrefixOf` dir] ++ replace "\\" "/" dir ++ "/"
            yield (name, url, lstrFromChunks [src])
    return (Map.fromList $ map ((,mempty{packageTags=[(T.pack "set",T.pack "all")]}) . fst) packages
           ,Set.fromList $ map fst packages, source)


readFregeOnline :: Timing -> Download -> IO (Map.Map String Package, Set.Set String, Source IO (String, URL, LStr))
readFregeOnline timing download = do
    frege <- download "frege-frege.txt" "http://try.frege-lang.org/hoogle-frege.txt"
    let source = do
            src <- liftIO $ strReadFile frege
            yield ("frege", "http://google.com/", lstrFromChunks [src])
    return (Map.empty, Set.singleton "frege", source)


readHaskellGhcpkg :: Timing -> Settings -> IO (Map.Map String Package, Set.Set String, Source IO (String, URL, LStr))
readHaskellGhcpkg timing settings = do
    cbl <- timed timing "Reading ghc-pkg" $ readGhcPkg settings
    let source =
            forM_ (Map.toList cbl) $ \(name,Package{..}) -> whenJust packageDocs $ \docs -> do
                let file = docs </> name <.> "txt"
                whenM (liftIO $ doesFileExist file) $ do
                    src <- liftIO $ strReadFile file
                    docs <- liftIO $ canonicalizePath docs
                    let url = "file://" ++ ['/' | not $ all isPathSeparator $ take 1 docs] ++
                              replace "\\" "/" (addTrailingPathSeparator docs)
                    yield (name, url, lstrFromChunks [src])
    cbl <- return $ let ts = map (both T.pack) [("set","stackage"),("set","installed")]
                    in Map.map (\p -> p{packageTags = ts ++ packageTags p}) cbl
    return (cbl, Map.keysSet cbl, source)


actionGenerate :: CmdLine -> IO ()
actionGenerate g@Generate{..} = withTiming (if debug then Just $ replaceExtension database "timing" else Nothing) $ \timing -> do
    putStrLn "Starting generate"
    createDirectoryIfMissing True $ takeDirectory database
    gcStats <- getGCStatsEnabled

    download <- return $ downloadInput timing insecure download (takeDirectory database)
    settings <- loadSettings
    (cbl, want, source) <- case language of
        Haskell | [""] <- local_ -> readHaskellGhcpkg timing settings
                | [] <- local_ -> readHaskellOnline timing settings download
                | otherwise -> readHaskellDirs timing local_
        Frege | null local_ -> errorIO "No support for local Frege databases"
              | otherwise -> readFregeOnline timing download
    let (cblErrs, popularity) = packagePopularity cbl
    want <- return $ if include /= [] then Set.fromList include else want

    (stats, _) <- storeWriteFile database $ \store -> do
        xs <- withBinaryFile (database `replaceExtension` "warn") WriteMode $ \warnings -> do
            hSetEncoding warnings utf8
            hPutStr warnings $ unlines cblErrs
            nCblErrs <- evaluate $ length cblErrs

            itemWarn <- newIORef 0
            let warning msg = do modifyIORef itemWarn succ; hPutStrLn warnings msg

            let consume :: Conduit (Int, (String, URL, LStr)) IO (Maybe Target, [Item])
                consume = awaitForever $ \(i, (pkg, url, body)) -> do
                    timedOverwrite timing ("[" ++ show i ++ "/" ++ show (Set.size want) ++ "] " ++ pkg) $
                        parseHoogle (\msg -> warning $ pkg ++ ":" ++ msg) url body

            writeItems store $ \items -> do
                let packages = [ fakePackage name $ "Not in Stackage, so not searched.\n" ++ T.unpack packageSynopsis
                               | (name,Package{..}) <- Map.toList cbl, name `Set.notMember` want]

                (seen, xs) <- runConduit $
                    source =$=
                    filterC (flip Set.member want . fst3) =$=
                        ((fmap Set.fromList $ mapC fst3 =$= sinkList) |$|
                        (((zipFromC 1 =$= consume) >> when (null include) (sourceList packages))
                            =$= pipelineC 10 (items =$= sinkList)))
                putStrLn ""

                let missing = [x | x <- Set.toList $ want `Set.difference` seen
                                 , fmap packageLibrary (Map.lookup x cbl) /= Just False]
                whenNormal $ when (missing /= []) $ do
                    putStrLn $ "Packages missing documentation: " ++ unwords (sortOn lower missing)
                when (Set.null seen) $
                    exitFail "No packages were found, aborting (use no arguments to index all of Stackage)"

                itemWarn <- readIORef itemWarn
                when (itemWarn > 0) $
                    putStrLn $ "Found " ++ show itemWarn ++ " warnings when processing items"
                return [(a,b) | (a,bs) <- xs, b <- bs]

        itemsMb <- if not gcStats then return 0 else do performGC; GCStats{..} <- getGCStats; return $ currentBytesUsed `div` (1024*1024)
        xs <- timed timing "Reodering items" $ return $! reorderItems settings (\s -> maybe 1 negate $ Map.lookup s popularity) xs
        timed timing "Writing tags" $ writeTags store (`Set.member` want) (\x -> maybe [] (map (both T.unpack) . packageTags) $ Map.lookup x cbl) xs
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
