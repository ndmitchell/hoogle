{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Generate(actionGenerate) where

import Data.List.Extra
import System.FilePath
import System.Directory.Extra
import System.IO.Extra
import Data.Tuple.Extra
import Control.Exception.Extra
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Extra
import Data.Monoid
import Data.Ord
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
import Action.CmdLine
import General.Conduit
import Control.DeepSeq

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
generate output metadata = ...
-}


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

type Download = String -> URL -> IO FilePath

readHaskellOnline :: Timing -> Settings -> Download -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellOnline timing settings download = do
    stackageLts <- download "haskell-stackage-lts.txt" "https://www.stackage.org/lts/cabal.config"
    stackageNightly <- download "haskell-stackage-nightly.txt" "https://www.stackage.org/nightly/cabal.config"
    platform <- download "haskell-platform.txt" "https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/src/Releases2015.hs"
    cabals   <- download "haskell-cabal.tar.gz" "https://hackage.haskell.org/packages/index.tar.gz"
    hoogles  <- download "haskell-hoogle.tar.gz" "https://hackage.haskell.org/packages/hoogle.tar.gz"

    -- peakMegabytesAllocated = 2
    setStackage <- Set.map strPack <$> (Set.union <$> setStackage stackageLts <*> setStackage stackageNightly)
    setPlatform <- Set.map strPack <$> setPlatform platform
    setGHC <- Set.map strPack <$> setGHC platform

    cbl <- timed timing "Reading Cabal" $ parseCabalTarball settings cabals
    let want = Set.insert (strPack "ghc") $ Set.unions [setStackage, setPlatform, setGHC]
    cbl <- pure $ flip Map.mapWithKey cbl $ \name p ->
        p{packageTags =
            [(strPack "set",strPack "included-with-ghc") | name `Set.member` setGHC] ++
            [(strPack "set",strPack "haskell-platform") | name `Set.member` setPlatform] ++
            [(strPack "set",strPack "stackage") | name `Set.member` setStackage] ++
            packageTags p}

    let source = do
            tar <- liftIO $ tarballReadFiles hoogles
            forM_ tar $ \(strPack . takeBaseName -> name, src) ->
                yield (name, hackagePackageURL name, src)
    pure (cbl, want, source)


readHaskellDirs
  :: Timing
  -> Settings
  -> Maybe FilePath
  -> [FilePath] -- ^ Prefix to remove from URLs to make the DB relocatable
  -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellDirs timing settings prefixToRemove dirs = do
    files <- concatMapM listFilesRecursive dirs
    -- We reverse/sort the list because of #206
    -- Two identical package names with different versions might be foo-2.0 and foo-1.0
    -- We never distinguish on versions, so they are considered equal when reordering
    -- So put 2.0 first in the list and rely on stable sorting. A bit of a hack.
    let order a = second Down $ parseTrailingVersion a
    let packages = map (strPack . takeBaseName &&& id) $ sortOn (map order . splitDirectories) $ filter ((==) ".txt" . takeExtension) files
    cabals <- mapM parseCabal $ filter ((==) ".cabal" . takeExtension) files
    let source = forM_ packages $ \(name, file) -> do
            src <- liftIO $ bstrReadFile file
            dir <- liftIO $ canonicalizePath $ takeDirectory file
            let url = case prefixToRemove of
                  Just prefix -> makeRelative prefix $ replace "\\" "/" dir ++ "/"
                  Nothing -> "file://" ++ ['/' | not $ "/" `isPrefixOf` dir] ++ replace "\\" "/" dir ++ "/"
            yield (name, url, lbstrFromChunks [src])
    pure (Map.union
                (Map.fromList cabals)
                (Map.fromListWith (<>) $ map generateBarePackage packages)
           ,Set.fromList $ map fst packages, source)
  where
    parseCabal fp = do
        src <- readFileUTF8' fp
        let pkg = readCabal settings src
        pure (strPack $ takeBaseName fp, pkg)

    generateBarePackage (name, file) =
        (name, mempty{packageTags = (strPack "set", strPack "all") : sets})
      where
        sets = map setFromDir $ filter (`isPrefixOf` file) dirs
        setFromDir dir = (strPack "set", strPack $ takeFileName $ dropTrailingPathSeparator dir)

readFregeOnline :: Timing -> Download -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readFregeOnline timing download = do
    frege <- download "frege-frege.txt" "http://try.frege-lang.org/hoogle-frege.txt"
    let source = do
            src <- liftIO $ bstrReadFile frege
            yield (strPack "frege", "http://google.com/", lbstrFromChunks [src])
    pure (Map.empty, Set.singleton $ strPack "frege", source)


readHaskellGhcpkg :: Timing -> Settings -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellGhcpkg timing settings = do
    cbl <- timed timing "Reading ghc-pkg" $ readGhcPkg settings
    let source =
            forM_ (Map.toList cbl) $ \(name,Package{..}) -> whenJust packageDocs $ \docs -> do
                let file = docs </> strUnpack name <.> "txt"
                whenM (liftIO $ doesFileExist file) $ do
                    src <- liftIO $ bstrReadFile file
                    docs <- liftIO $ canonicalizePath docs
                    let url = "file://" ++ ['/' | not $ all isPathSeparator $ take 1 docs] ++
                              replace "\\" "/" (addTrailingPathSeparator docs)
                    yield (name, url, lbstrFromChunks [src])
    cbl <- pure $ let ts = map (both strPack) [("set","stackage"),("set","installed")]
                    in Map.map (\p -> p{packageTags = ts ++ packageTags p}) cbl
    pure (cbl, Map.keysSet cbl, source)

-- | @hoogle generate --haddock=path/to/doc@ works similarly to @hoogle generate --local@,
-- getting package info from a local ghc-pkg database, but uses @path/to/doc@
-- as the canonical source of Haddock documentation.
-- @path/to/doc@ should be a path to the directory that contains the root @index.html@
-- generated by cabal haddock or stack haddock.
-- Paths stored in the database are relative to this root.
--
-- @hoogle server --haddock=path/to/doc@ is intended to be used with a database
-- containing relative paths. In contrast to @hoogle server --local@,
-- paths relative to the filesystem root are not allowed;
-- only files from @path/to/doc@ are served.
--
-- These features are intended to be used together, when you want to
-- create a Hoogle index for your project in one place, then serve it somewhere else.
-- This enables a workflow such as the following:
--
-- 1. Run cabal haddock or stack haddock on your build server
-- 2. Run hoogle generate --haddock=path/to/doc on your build server (this assumes you know where the docs were written)
-- 3. Deploy the Hoogle database and Haddock files to your documentation server & run hoogle server --haddock=new/path/to/doc
--
-- from https://github.com/ndmitchell/hoogle/pull/202
readHaskellHaddock :: Timing -> Settings -> FilePath -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellHaddock timing settings docBaseDir = do
    cbl <- timed timing "Reading ghc-pkg" $ readGhcPkg settings
    let source =
            forM_ (Map.toList cbl) $ \(name, p@Package{..}) -> do
                let docs = docDir (strUnpack name) p
                    file = docBaseDir </> docs </> (strUnpack name) <.> "txt"
                whenM (liftIO $ doesFileExist file) $ do
                    src <- liftIO $ bstrReadFile file
                    let url = ['/' | not $ all isPathSeparator $ take 1 docs] ++
                              replace "\\" "/" (addTrailingPathSeparator docs)
                    yield (name, url, lbstrFromChunks [src])
    cbl <- pure $ let ts = map (both strPack) [("set","stackage"),("set","installed")]
                    in Map.map (\p -> p{packageTags = ts ++ packageTags p}) cbl
    pure (cbl, Map.keysSet cbl, source)

    where docDir name Package{..} = name ++ "-" ++ strUnpack packageVersion

actionGenerate :: CmdLine -> IO ()
actionGenerate g@Generate{..} = withTiming (if debug then Just $ replaceExtension database "timing" else Nothing) $ \timing -> do
    putStrLn "Starting generate"
    createDirectoryIfMissing True $ takeDirectory database
    whenLoud $ putStrLn $ "Generating files to " ++ takeDirectory database

    let warnFlagIgnored thisFlag reason ignoredFlagPred ignoredFlag =
          when ignoredFlagPred $ putStrLn $ "Warning: " <> thisFlag <> " is " <> reason <> ", which means " <> ignoredFlag <> " is ignored."

    let doDownload name url = do
          let download' = case download of
                Just True -> AlwaysDownloadInput
                Just False -> NeverDownloadInput
                Nothing -> DownloadInputIfNotThere
          downloadInput timing insecure download' (takeDirectory database) name url

    settings <- loadSettings
    (cbl, want, source) <- case language of
        Haskell | Just dir <- haddock -> do
                    warnFlagIgnored "--haddock" "set" (local_ /= []) "--local"
                    warnFlagIgnored "--haddock" "set" (isJust download) "--download"
                    warnFlagIgnored "--haddock" "set" relocatable "--relocatable"
                    readHaskellHaddock timing settings dir
                | [""] <- local_ -> do
                    warnFlagIgnored "--local" "used as flag (no paths)" (isJust download) "--download"
                    readHaskellGhcpkg timing settings
                | [] <- local_ -> do readHaskellOnline timing settings doDownload
                | relocatable, _:_:_ <- local_ ->
                    exitFail "Error: --relocatable needs exactly one --local, or the paths will be ambiguous"
                | relocatable -> do
                    prefix <- traverse canonicalizePath $ listToMaybe local_
                    readHaskellDirs timing settings prefix local_
                | otherwise -> readHaskellDirs timing settings Nothing local_
        Frege | [] <- local_ -> readFregeOnline timing doDownload
              | otherwise -> errorIO "No support for local Frege databases"
    (cblErrs, popularity) <- evaluate $ packagePopularity cbl
    cbl <- evaluate $ Map.map (\p -> p{packageDepends=[]}) cbl -- clear the memory, since the information is no longer used
    evaluate popularity

    want <- pure $ if include /= [] then Set.fromList $ map strPack include else want
    want <- pure $ case count of Nothing -> want; Just count -> Set.fromList $ take count $ Set.toList want

    (stats, _) <- storeWriteFile database $ \store -> do
        xs <- withBinaryFile (database `replaceExtension` "warn") WriteMode $ \warnings -> do
            hSetEncoding warnings utf8
            hPutStr warnings $ unlines cblErrs
            nCblErrs <- evaluate $ length cblErrs

            itemWarn <- newIORef 0
            let warning msg = do modifyIORef itemWarn succ; hPutStrLn warnings msg

            let consume :: ConduitM (Int, (PkgName, URL, LBStr)) (Maybe Target, [Item]) IO ()
                consume = awaitForever $ \(i, (strUnpack -> pkg, url, body)) -> do
                    timedOverwrite timing ("[" ++ show i ++ "/" ++ show (Set.size want) ++ "] " ++ pkg) $
                        parseHoogle (\msg -> warning $ pkg ++ ":" ++ msg) url body

            writeItems store $ \items -> do
                xs <- runConduit $
                    source .|
                    filterC (flip Set.member want . fst3) .|
                    void ((|$|)
                        (zipFromC 1 .| consume)
                        (do seen <- fmap Set.fromList $ mapMC (evaluate . force . strCopy . fst3) .| sinkList
                            let missing = [x | x <- Set.toList $ want `Set.difference` seen
                                             , fmap packageLibrary (Map.lookup x cbl) /= Just False]
                            liftIO $ putStrLn ""
                            liftIO $ whenNormal $ when (missing /= []) $ do
                                putStrLn $ "Packages missing documentation: " ++ unwords (sortOn lower $ map strUnpack missing)
                            liftIO $ when (Set.null seen) $
                                exitFail "No packages were found, aborting (use no arguments to index all of Stackage)"
                            -- synthesise things for Cabal packages that are not documented
                            forM_ (Map.toList cbl) $ \(name, Package{..}) -> when (name `Set.notMember` seen) $ do
                                let ret prefix = yield $ fakePackage name $ prefix ++ trim (strUnpack packageSynopsis)
                                if name `Set.member` want then
                                    (if packageLibrary
                                        then ret "Documentation not found, so not searched.\n"
                                        else ret "Executable only. ")
                                else if null include then
                                    ret "Not on Stackage, so not searched.\n"
                                else
                                    pure ()
                            ))
                    .| pipelineC 10 (items .| sinkList)

                itemWarn <- readIORef itemWarn
                when (itemWarn > 0) $
                    putStrLn $ "Found " ++ show itemWarn ++ " warnings when processing items"
                pure [(a,b) | (a,bs) <- xs, b <- bs]

        itemsMemory <- getStatsCurrentLiveBytes
        xs <- timed timing "Reordering items" $ pure $! reorderItems settings (\s -> maybe 1 negate $ Map.lookup s popularity) xs
        timed timing "Writing tags" $ writeTags store (`Set.member` want) (\x -> maybe [] (map (both strUnpack) . packageTags) $ Map.lookup x cbl) xs
        timed timing "Writing names" $ writeNames store xs
        timed timing "Writing types" $ writeTypes store (if debug then Just $ dropExtension database else Nothing) xs

        x <- getVerbosity
        when (x >= Loud) $
            whenJustM getStatsDebug print
        when (x >= Normal) $ do
            whenJustM getStatsPeakAllocBytes $ \x ->
                putStrLn $ "Peak of " ++ x ++ ", " ++ fromMaybe "unknown" itemsMemory ++ " for items"

    when debug $
        writeFile (database `replaceExtension` "store") $ unlines stats
