
module Recipe.Download(download) where

import General.Base
import General.System
import Recipe.Type


-- download everything required for the recipes
download :: CmdLine -> IO ()
download opt = do
    createDirectoryIfMissing True "download"
    wget opt keywords "http://haskell.org/haskellwiki/Keywords"
    wget opt platform "http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal"
    wget opt "download/base.txt" "http://haskell.org/hoogle/base.txt"
    wget opt "download/ghc.txt" "http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/ghc.txt"
    downloadTarball opt cabals "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    downloadTarball opt inputs "http://hackage.haskell.org/packages/archive/00-hoogle.tar.gz"


check :: String -> URL -> IO ()
check name url | isWindows = do
    res <- findExecutable name
    when (isNothing res) $ putStrLn $
        "WARNING: Could not find command line program " ++ name ++ ".\n" ++
        "  You may be able to install it from:\n  " ++ url
check _ _ = return ()


wgetMay :: CmdLine -> FilePath -> URL -> IO Bool
wgetMay opt fil url = do
    b <- doesFileExist fil
    when (not b || redownload opt) $ do
        check "wget" "http://gnuwin32.sourceforge.net/packages/wget.htm"
        let sys cmd = do
                res <- fmap (== ExitSuccess) $ system cmd
                unless res $ do
                    b <- doesFileExist fil
                    when b $ removeFile fil
                return res
        b <- sys $ "wget " ++ url ++ " --output-document=" ++ fil
        unless b $ do sys $ "curl " ++ url ++ " --output " ++ fil; return ()
    doesFileExist fil


wget :: CmdLine -> FilePath -> URL -> IO ()
wget opt fil url = do
    b <- wgetMay opt fil url
    unless b $ error $ "Failed to download " ++ url


downloadTarball :: CmdLine -> FilePath -> URL -> IO ()
downloadTarball opt out url = do
    b <- doesFileExist $ out <.> "txt"
    when (not b || redownload opt) $ do
        wget opt (out <.> "tar.gz") url
        createDirectoryIfMissing True out
        withDirectory out $ do
            check "gzip" "http://gnuwin32.sourceforge.net/packages/gzip.htm"
            check "tar" "http://gnuwin32.sourceforge.net/packages/gtar.htm"
            putStrLn "Extracting tarball... "
            system_ $ "gzip --decompress --force .." </> takeFileName out <.> "tar.gz"
            system_ $ "tar -xf .." </> takeFileName out <.> "tar"
            putStrLn "Finished extracting tarball"
        writeFile (out <.> "txt") ""
