{-# LANGUAGE RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, GeneralizedNewtypeDeriving, CPP #-}

module Recipe.All(recipes) where

import General.Base hiding (readFile')
import General.System as Sys
import General.Util
import Control.Concurrent
import Control.Exception as E
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Recipe.Haddock

import Recipe.Command
import Recipe.Keyword
import Recipe.Hackage
import Recipe.Cabal
import Hoogle
import qualified Paths_hoogle as V
import Data.Version
import CmdLine.All as C


-- CmdLine is guaranteed to be a constructor of type Data
recipes :: C.CmdLine -> IO ()
recipes opt@Data{..} = withModeGlobalRead $ do
    hSetBuffering stdout NoBuffering
    createDirectoryIfMissing True datadir
    withDirectory datadir $ do
        when redownload $ do
            if nodownload
              then error "Downloads are disabled, cannot re-download"
              else forM_ (urls opt) $ \(file,_) -> removeFile_ $ "downloads" </> file
        when rebuild $ removeFile ".shake.database"
        (count, file) <- withWarnings $ \warn ->
            shake shakeOptions{shakeVersion=showVersion V.version, shakeThreads=threads, shakeProgress=progressSimple} $
                rules opt warn
        putStrLn $ show count ++ " warnings, saved to " ++ file
        putStrLn "Data generation complete"


newtype CabalVersion = CabalVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype HoogleVersion = HoogleVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

rules :: C.CmdLine -> ([String] -> IO ()) -> Rules ()
rules opts@Data{..} warn = do
    let srcCabal name ver = "downloads/cabal" </> name </> ver </> name <.> "cabal"
    let srcHoogle name ver = "downloads/hoogle" </> name </> ver </> "doc" </> "html" </> name <.> "txt"

    (\x -> "downloads/*" ?== x &&
           isJust (lookup (takeFileName x) (urls opts))) ?> \out -> do
        when nodownload $
             error "Downloads are disabled; you need to acquire the source files manually."
        let Just url = lookup (takeFileName out) (urls opts)
        putNormal $ "Downloading " ++ out
        wget opts url out
        putNormal $ "Downloaded " ++ out

    "downloads/*.cache" %> \out -> do
        let src = dropExtension out
        need [src]
        src <- liftIO $ readFileUtf8' src
        b <- liftIO $ Sys.doesFileExist out
        liftIO $ if not b then writeFileUtf8 out src else do
            old <- readFileUtf8' out
            when (src /= old) $ writeFileUtf8 out src

    "//*.tar" %> \out -> do
        let src = out <.> "gz"
        need [src]
        ungzip src out

    "//*.index" %> \out -> do
        let src = out -<.> "tar"
        need [src]
        putNormal $ "Extracting tar file " ++ out
        tarExtract src
        putNormal $ "Finished extracting tar file " ++ out
        writeFileChanged out . unlines =<< tarList src

    index <- newCache $ \index -> do
        xs <- readFileLines index
        let asVer = map (read :: String -> Int) . words . map (\x -> if x == '.' then ' ' else x)
        return $ Map.fromListWith (\a b -> if asVer a > asVer b then a else b)
            [(name, ver) | x <- xs, let name = takeDirectory1 x, let ver = takeDirectory1 $ dropDirectory1 x, all (\x -> isDigit x || x == '.') ver]

    verCabal  <- addOracle $ \(CabalVersion  x) -> fmap (Map.lookup x) $ index "downloads/cabal.index"
    verHoogle <- addOracle $ \(HoogleVersion x) -> fmap (Map.lookup x) $ index "downloads/hoogle.index"

    if null actions then want ["default.hoo"] else action $ do
        (good,bad) <- partitionM (fmap isJust . verHoogle . HoogleVersion) actions
        forM_ (delete "all" bad) $ \x -> putNormal $ "Couldn't generate database for " ++ x ++ ", no Hoogle docs available"
        need $ map (<.> "hoo") $ ["all" | "all" `elem` bad] ++ good

    alternatives $ do -- Match *.txt
        "keyword.txt" %> \out -> do
            let src = "downloads/keyword.htm.cache"
            need [src]
            contents <- liftIO $ readFileUtf8' src
            liftIO $ writeFileUtf8 out $ translateKeywords contents

        "default.txt" %> \out -> do
            writeFileLines out ["@combine keyword","@combine package","@combine platform"]

        "platform.txt" %> \out -> do
            contents <- readFile' "downloads/platform.cabal.cache"
            writeFileLines out ["@combine " ++ x | x <- platformPackages contents]

        "package.txt" %> \out -> do
            cabs <- index "downloads/cabal.index"
            xs <- liftIO $ forM (Map.toList cabs) $ \(name,ver) -> do
                src <- try $ readCabal $ srcCabal name ver
                return $ case src of
                    Left (_ :: SomeException) -> []
                    Right src ->
                        [""] ++ zipWith (++) ("-- | " : repeat "--   ") (cabalDescription src) ++
                        ["--","-- Version " ++ ver, "@url package/" ++ name, "@entry package " ++ name]
            liftIO $ writeFileUtf8 out $ unlines $ ("@url " ++ hackage) : "@package package" : concat xs

        "*.txt" %> \out -> do
            let name = takeBaseName out
                base = name == "base"
            cab <- fmap (fmap $ srcCabal name) $ verCabal (CabalVersion name)
            hoo <- if base
                   then need ["downloads/base.txt.cache"] >> return (Just "downloads/base.txt.cache")
                   else fmap (fmap $ srcHoogle name) $ verHoogle (HoogleVersion name)
            hoo <- return $ fromMaybe (error $ "Couldn't find hoogle file for " ++ name) hoo
            hoo <- liftIO $ readFileUtf8' hoo `E.catch` \(_ :: SomeException) -> readFileLatin1' hoo
            deps <- liftIO $ case cab of
                Nothing -> return []
                Just cab -> do
                    res <- try $ readCabal cab
                    case res of
                        Left (err :: SomeException) -> do warn [takeBaseName cab ++ ": failed to read cabal file, " ++ cab ++ ", " ++ show err]; return []
                        Right x -> return $ cabalDepends x
            let cleanDeps = deps \\ (name:avoid)
            loc <- liftIO $ findLocal local name
            liftIO $ writeFileUtf8 out $ unlines $
                ["@depends " ++ a | a <- cleanDeps] ++ haddockHacks loc (lines hoo)

    alternatives $ do -- Match *.hoo
        phony "all.hoo" $ do
            pkgs <- index "downloads/hoogle.index"
            need $ map (<.> "hoo") $ "default" : Map.keys pkgs

        imported <- newCache $ \file -> do
            need [file]
            xs <- liftIO $ readFileUtf8' file
            return [x | x <- lines xs, takeWhile (not . isSpace) x `elem` ["type","data","newtype","class","instance","@depends"]]
        let splitDeps = first (map $ drop 9) . span ("@depends " `isPrefixOf`)

        let genImported seen [] = return []
            genImported seen (t:odo) = do
                v <- if t `Set.member` seen then return Nothing else verHoogle $ HoogleVersion t
                if isNothing v then genImported seen odo else do
                    i <- imported $ t <.> "txt"
                    fmap (i++) $ genImported (Set.insert t seen) (fst (splitDeps i) ++ odo)

        "*.hoo" %> \out -> do
            let src = out -<.> "txt"
            need [src]
            contents <- liftIO $ fmap lines $ readFileUtf8' src
            if not (null contents) && "@combine " `isPrefixOf` head contents then do
                let deps = [x <.> "hoo" | x <- contents, Just x <- [stripPrefix "@combine " x]]
                need deps
                putNormal $ "Creating " ++ out ++ " from " ++ show (length deps) ++ " databases... "
                liftIO $ mergeDatabase deps out
             else do
                (deps, contents) <- return $ splitDeps contents
                deps <- genImported (Set.singleton $ takeBaseName out) deps
                putNormal $ "Creating " ++ out ++ "... "
                liftIO $ createDatabase hackage Haskell [] (unlines deps) $ out -<.> "dep"
                deps <- liftIO $ loadDatabase $ out -<.> "dep"
                err <- liftIO $ createDatabase hackage Haskell [deps] (unlines contents) out
                liftIO $ warn [takeBaseName out ++ ": " ++ show e | e <- err]


urls :: C.CmdLine -> [(FilePath, URL)]
urls Data{..} = let (*) = (,) in
    ["keyword.htm" * "http://wiki.haskell.org/Keywords"
    ,"platform.cabal" * "http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal"
    ,"base.txt" * "http://www.haskell.org/hoogle/base.txt"
    ,"cabal.tar.gz" * (hackage ++ "packages/index.tar.gz")
    ,"hoogle.tar.gz" * (hackage ++ "packages/hoogle.tar.gz")]


withWarnings :: (([String] -> IO ()) -> IO ()) -> IO (Int, FilePath)
withWarnings act = do
    count <- newMVar 0
    let file = ".warnings"
    writeFile file ""
    act $ \xs -> unless (null xs) $ modifyMVar_ count $ \i -> do
        appendFile file $ unlines xs
        return $! i + length xs
    i <- readMVar count
    return (i, file)
