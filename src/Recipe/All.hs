{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Recipe.All(recipes) where

import General.Base hiding (readFile')
import General.System as Sys
import Control.Exception as E
import qualified Data.Set as Set
import Development.Shake
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
        resetWarnings
        when redownload $ do
            forM_ urls $ \(file,_) -> removeFile_ $ "downloads" </> file
        shake shakeOptions{shakeVersion=showVersion V.version, shakeThreads=threads, shakeProgress=progressSimple} $ do
            want $ map (<.> "hoo") $ if null actions then ["default"] else actions
            rules opt
        recapWarnings
        putStrLn "Data generation complete"


rules :: CmdLine -> Rules ()
rules Data{..} = do
    "downloads/packages.txt" *> \out -> do
        need ["downloads/hoogle.untar","downloads/cabal.untar"]
        as <- liftIO $ listing "downloads/hoogle"
        bs <- liftIO $ listing "downloads/cabal"
        writeFileLines out $ Set.toList $ Set.fromList as `Set.intersection` Set.fromList bs

    packages <- newCache $ \() -> fmap (Set.fromList . lines) $ readFile' "downloads/packages.txt"

    "//*.tar" *> \out -> do
        let src = out <.> "gz"
        need [src]
        ungzip src out

    "//*.untar" *> \out -> do
        let src = out -<.> "tar"
        need [src]
        tarExtract src
        writeFile' out ""

    (\x -> "downloads/*" ?== x && isJust (lookup (takeFileName x) urls)) ?> \out -> do
        let Just url = lookup (takeFileName out) urls
        -- liftIO $ copyFile ("C:/spacework/hoogle/cache" </> takeFileName out) out
        wget url out

    alternatives $ do -- *.txt
        "keyword.txt" *> \out -> do
            let src = "downloads/keyword.htm"
            need [src]
            contents <- liftIO $ readFileUtf8' src
            liftIO $ writeFileUtf8 out $ translateKeywords contents

        "default.txt" *> \out -> do
            writeFileLines out ["@combine keyword","@combine package","@combine platform"]

        "platform.txt" *> \out -> do
            let src = "downloads/platform.cabal"
            contents <- readFile' src
            writeFileLines out ["@combine " ++ x | x <- platformPackages contents]

        "package.txt" *> \out -> do
            need ["downloads/cabal.untar"]
            xs <- liftIO $ listing "downloads/cabal"
            xs <- liftIO $ forM xs $ \name -> do
                ver <- version "downloads/cabal" name
                let file = "downloads/cabal" </> name </> ver </> name <.> "cabal"
                src <- readCabal file
                return $ case src of
                    Nothing -> []
                    Just src ->
                        [""] ++ zipWith (++) ("-- | " : repeat "--   ") (cabalDescription src) ++
                        ["--","-- Version " ++ ver, "@url package/" ++ name, "@entry package " ++ name]
            liftIO $ writeFileUtf8 out $ unlines $ "@url http://hackage.haskell.org/" : "@package package" : concat xs

        "*.txt" *> \out -> do
            need ["downloads/hoogle.untar","downloads/cabal.untar"]
            let name = takeBaseName out
                base = name == "base"
            vc <- liftIO $ version "downloads/cabal" name
            vh <- liftIO $ if base then return vc else version "downloads/hoogle" name
            let hoo = if base then "downloads/base.txt" else "downloads/hoogle" </> name </> vh </> "doc" </> "html" </> name <.> "txt"
                cab = "downloads/cabal" </> name </> vc </> name <.> "cabal"
            need [hoo, cab]
            hoo <- liftIO $ readFileUtf8' hoo `E.catch` \(_ :: SomeException) -> readFile hoo
            deps <- liftIO $ fmap (maybe [] cabalDepends) $ readCabal cab
            let cleanDeps = deps \\ (name:avoid)
            loc <- liftIO $ findLocal local name
            liftIO $ writeFileUtf8 out $ unlines $
                ["@depends " ++ a | a <- cleanDeps] ++ haddockHacks loc (lines hoo)

    alternatives $ do -- *.hoo
        phony "all.hoo" $ do
            pkgs <- packages ()
            need $ map (<.> "hoo") $ "default" : Set.toList pkgs

        imported <- newCache $ \file -> do
            need [file]
            xs <- liftIO $ readFileUtf8' file
            return [x | x <- lines xs, takeWhile (not . isSpace) x `elem` ["type","data","newtype","class","instance","@depends"]]
        let listDeps = map (drop 9) . takeWhile ("@depends " `isPrefixOf`)

        let genImported pkgs seen [] = return []
            genImported pkgs seen (t:odo)
                | t `Set.member` seen || not (t `Set.member` pkgs) = genImported pkgs seen odo
                | otherwise = do
                    i <- imported $ t <.> "txt"
                    let deps = listDeps i
                    fmap (i++) $ genImported pkgs (Set.insert t seen) (deps++odo)

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
                pkgs <- packages ()
                deps <- genImported pkgs (Set.singleton $ takeBaseName out) $ listDeps contents
                let (err,db) = createDatabase Haskell [snd $ createDatabase Haskell [] $ unlines deps] $ unlines contents
                unless (null err) $ putNormal $ "Skipped " ++ show (length err) ++ " warnings in " ++ out
                putLoud $ unlines $ map show err
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


listing :: FilePath -> IO [String]
listing dir = do
    xs <- Sys.getDirectoryContents dir
    return $ sortBy (comparing $ map toLower) $ filter (`notElem` [".","..","preferred-versions"]) xs

version :: FilePath -> String -> IO String
version dir x = do
    ys <- Sys.getDirectoryContents $ dir </> x
    when (null ys) $ error $ "Couldn't find version for " ++ x ++ " in " ++ dir
    let f = map (read :: String -> Int) . words . map (\x -> if x == '.' then ' ' else x)
    return $ maximumBy (comparing f) $ filter (all (not . isAlpha)) ys
