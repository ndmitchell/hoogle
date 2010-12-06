
module Recipe.Download(download) where

import Recipe.Type
import General.Base
import System.Cmd
import System.Exit
import System.Directory


-- download everything required for the recipes
download :: CmdLine -> IO ()
download opt = do
    createDirectoryIfMissing True "download"
    wget opt keywords "http://haskell.org/haskellwiki/Keywords"
    wget opt platform "http://code.haskell.org/haskell-platform/haskell-platform.cabal"
    downloadTarball opt cabals "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    if haddock opt then downloadHaddocks opt
                   else downloadTarball opt haddocks "http://haskell.org/hoogle/hackage-haddock.tar.gz"


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
            system_ $ "tar -xf .." </> takeFileName out <.> "tar.gz"
        writeFile (out <.> "txt") ""


downloadHaddocks :: CmdLine -> IO ()
downloadHaddocks opt = do
    b <- doesFileExist $ haddocks <.> "txt"
    unless b $ do
        xs <- listing cabals
        forM_ xs $ \name -> do
            ver <- version cabals name
            let out = haddocks </> name </> ver </> name <.> "txt"
                url = "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ ver ++ "/doc/html/" ++ name ++ ".txt"
            createDirectoryIfMissing True $ takeDirectory out
            b <- wgetMay opt out url
            unless b $ writeFile out ""
        withDirectory haddocks $ system_ "tar -czf../hackage-haddock.tar.gz *"
        writeFile (haddocks <.> "txt") ""
