{-# LANGUAGE RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Recipe.All(recipes) where

import General.Base hiding (readFile')
import General.System as Sys
import Control.Exception as E
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Recipe.Haddock

import Recipe.Warning
import Recipe.Command
import Recipe.Keyword
import Recipe.Hackage
import Recipe.Cabal
import Hoogle
import qualified Paths_hoogle as V
import Data.Version
import CmdLine.All


-- CmdLine is guaranteed to be a constructor of type Data
recipes :: CmdLine -> IO ()
recipes opt@Data{..} = withModeGlobalRead $ do
    hSetBuffering stdout NoBuffering
    createDirectoryIfMissing True datadir
    withDirectory datadir $ do
        when redownload $ do
            forM_ urls $ \(file,_) -> removeFile_ $ "downloads" </> file
        (count, file) <- withWarnings $ \warn ->
            shake shakeOptions{shakeVersion=showVersion V.version, shakeThreads=threads, shakeProgress=progressSimple} $ do
                want $ map (<.> "hoo") $ if null actions then ["default"] else actions
                rules opt warn
        putStrLn $ show count ++ " warnings, saved to " ++ file
        putStrLn "Data generation complete"


newtype CabalVersion = CabalVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype HoogleVersion = HoogleVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

rules :: CmdLine -> ([String] -> IO ()) -> Rules ()
rules Data{..} warn = do
    let srcCabal name ver = "downloads/cabal" </> name </> ver </> name <.> "cabal"
    let srcHoogle name ver = "downloads/hoogle" </> name </> ver </> "doc" </> "html" </> name <.> "txt"

    (\x -> "downloads/*" ?== x && isJust (lookup (takeFileName x) urls)) ?> \out -> do
        let Just url = lookup (takeFileName out) urls
        putNormal $ "Downloading " ++ out
        -- liftIO $ copyFile ("C:/spacework/hoogle/cache" </> takeFileName out) out
        wget url out
        putNormal $ "Downloaded " ++ out

    "//*.tar" *> \out -> do
        let src = out <.> "gz"
        need [src]
        ungzip src out

    "//*.index" *> \out -> do
        let src = out -<.> "tar"
        need [src]
        putNormal $ "Extracting tar file " ++ out
        tarExtract src
        putNormal $ "Finished extracting tar file " ++ out
        writeFileLines out =<< tarList src

    index <- newCache $ \index -> do
        xs <- readFileLines index
        let asVer = map (read :: String -> Int) . words . map (\x -> if x == '.' then ' ' else x)
        return $ Map.fromListWith (\a b -> if asVer a > asVer b then a else b)
            [(name, ver) | x <- xs, let name = takeDirectory1 x, let ver = takeDirectory1 $ dropDirectory1 x, all (\x -> isDigit x || x == '.') ver]

    verCabal  <- addOracle $ \(CabalVersion  x) -> fmap (Map.lookup x) $ index "downloads/cabal.index"
    verHoogle <- addOracle $ \(HoogleVersion x) -> fmap (Map.lookup x) $ index "downloads/hoogle.index"

    alternatives $ do -- *.txt
        "keyword.txt" *> \out -> do
            let src = "downloads/keyword.htm"
            need [src]
            contents <- liftIO $ readFileUtf8' src
            liftIO $ writeFileUtf8 out $ translateKeywords contents

        "default.txt" *> \out -> do
            writeFileLines out ["@combine keyword","@combine package","@combine platform"]

        "platform.txt" *> \out -> do
            contents <- readFile' "downloads/platform.cabal"
            writeFileLines out ["@combine " ++ x | x <- platformPackages contents]

        "package.txt" *> \out -> do
            cabs <- index "downloads/cabal.index"
            xs <- liftIO $ forM (Map.toList cabs) $ \(name,ver) -> do
                src <- try $ readCabal $ srcCabal name ver
                return $ case src of
                    Left (_ :: SomeException) -> []
                    Right src ->
                        [""] ++ zipWith (++) ("-- | " : repeat "--   ") (cabalDescription src) ++
                        ["--","-- Version " ++ ver, "@url package/" ++ name, "@entry package " ++ name]
            liftIO $ writeFileUtf8 out $ unlines $ "@url http://hackage.haskell.org/" : "@package package" : concat xs

        "*.txt" *> \out -> do
            let name = takeBaseName out
                base = name == "base"
            cab <- fmap (fmap $ srcCabal name) $ verCabal (CabalVersion name)
            hoo <- if base
                   then need ["downloads/base.txt"] >> return (Just "downloads/base.txt")
                   else fmap (fmap $ srcHoogle name) $ verHoogle (HoogleVersion name)
            hoo <- return $ fromMaybe (error $ "Couldn't find hoogle file for " ++ name) hoo
            hoo <- liftIO $ readFileUtf8' hoo `E.catch` \(_ :: SomeException) -> readFile hoo
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

    alternatives $ do -- *.hoo
        phony "all.hoo" $ do
            pkgs <- index "downloads/hoogle.index"
            need $ map (<.> "hoo") $ "default" : Map.keys pkgs

        imported <- newCache $ \file -> do
            need [file]
            xs <- liftIO $ readFileUtf8' file
            return [x | x <- lines xs, takeWhile (not . isSpace) x `elem` ["type","data","newtype","class","instance","@depends"]]
        let listDeps = map (drop 9) . takeWhile ("@depends " `isPrefixOf`)

        let genImported seen [] = return []
            genImported seen (t:odo) = do
                v <- if t `Set.member` seen then return Nothing else verHoogle $ HoogleVersion t
                if isNothing v then genImported seen odo else do
                    i <- imported $ t <.> "txt"
                    fmap (i++) $ genImported (Set.insert t seen) (listDeps i++odo)

        "*.hoo" *> \out -> do
            let src = out -<.> "txt"
            need [src]
            contents <- liftIO $ fmap lines $ readFileUtf8' src
            if not (null contents) && "@combine " `isPrefixOf` head contents then do
                let deps = [x <.> "hoo" | x <- contents, Just x <- [stripPrefix "@combine " x]]
                need deps
                dbs <- liftIO $ mapM loadDatabase deps
                putNormal $ "Creating " ++ out ++ " from " ++ show (length deps) ++ " databases... "
                liftIO $ performGC
                liftIO $ saveDatabase out $ mconcat dbs
             else do
                deps <- genImported (Set.singleton $ takeBaseName out) $ listDeps contents
                let (err,db) = createDatabase Haskell [snd $ createDatabase Haskell [] $ unlines deps] $ unlines contents
                liftIO $ warn [takeBaseName out ++ ": " ++ show e | e <- err]
                putNormal $ "Creating " ++ out ++ "... "
                liftIO $ performGC
                liftIO $ saveDatabase out db


urls :: [(FilePath, URL)]
urls = let (*) = (,) in
    ["keyword.htm" * "http://www.haskell.org/haskellwiki/Keywords"
    ,"platform.cabal" * "http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal"
    ,"base.txt" * "http://www.haskell.org/hoogle/base.txt"
    ,"ghc.txt" * "http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/ghc.txt"
    ,"cabal.tar.gz" * "http://hackage.haskell.org/packages/index.tar.gz"
    ,"hoogle.tar.gz" * "http://hackage.haskell.org/packages/hoogle.tar.gz"]
