
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
    wget opt inputBase "http://haskell.org/hoogle/base.txt"
    downloadTarball opt cabals "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    downloadTarball opt inputs "http://hackage.haskell.org/packages/archive/00-hoogle.tar.gz"


wgetMay :: CmdLine -> FilePath -> URL -> IO Bool
wgetMay opt fil url = do
    b <- doesFileExist fil
    when (not b || redownload opt) $ do
        res <- system $ "wget " ++ url ++ " -O " ++ fil
        let b = res == ExitSuccess
        unless b $ removeFile fil
    doesFileExist fil


wget :: CmdLine -> FilePath -> URL -> IO ()
wget opt fil url = do
    b <- wgetMay opt fil url
    unless b $ error $ "Failed to download " ++ url


downloadTarball :: CmdLine -> FilePath -> URL -> IO ()
downloadTarball opt out url = do
    b <- doesFileExist $ out <.> "txt"
    unless b $ do
        wget opt (out <.> "tar.gz") url
        createDirectoryIfMissing True out
        withDirectory out $
            system_ $ "tar -xzf .." </> takeFileName out <.> "tar.gz"
        writeFile (out <.> "txt") ""
