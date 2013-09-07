
module Recipe.Download(download) where

import General.Base
import General.System
import Recipe.Type

type Downloader = FilePath -> URL -> String

wget :: Downloader
wget fp url = "wget -nv " ++ url ++ " --output-document=" ++ fp
curl :: Downloader
curl fp url = "curl -sSL " ++ url ++ " --output " ++ fp

findDownloader :: IO Downloader
findDownloader = do
    dl <- liftM2 mplus (check "wget") (check "curl")
    when (isNothing dl) $ error "Could not find downloader, neither curl nor wget are on the $PATH."
    return $ matchDl (fromJust dl)
    where matchDl d | "wget" `isInfixOf` d = wget
                    | "curl" `isInfixOf` d = curl

withDownloader :: CmdLine -> Downloader -> [(FilePath, FilePath, URL)] -> IO ()
withDownloader opt downloader items =
    let sys = fmap (== ExitSuccess) . system
        download (f, f', u) = do
            b <- doesFileExist f
            when (not b || redownload opt) $ do
                res <- sys (downloader f' u)
                unless res $ do
                    b <- doesFileExist f'
                    when b $ removeFile f'
                    error $ "Failed to download: " ++ u
            doesFileExist f'
    in forM_ items download

-- download everything required for the recipes
download :: CmdLine -> IO ()
download opt = do
    createDirectoryIfMissing True "download"
    downloader <- findDownloader
    let items = [ (keywords, keywords, "http://www.haskell.org/haskellwiki/Keywords")
                , (platform, platform, "http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal")
                , ("download/base.txt", "download/base.txt", "http://www.haskell.org/hoogle/base.txt")
                , ("download/ghc.txt",  "download/ghc.txt", "http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/ghc.txt")
                , (cabals <.> "txt", cabals <.> "tar.gz", "http://hackage.haskell.org/packages/archive/00-index.tar.gz")
                , (inputs <.> "txt", inputs <.> "tar.gz", "http://hackage.haskell.org/packages/archive/00-hoogle.tar.gz")
                ]
    withDownloader opt downloader items
    extractTarball cabals
    extractTarball inputs


check :: String -> IO (Maybe FilePath)
check name = do
    res <- findExecutable name
    when (isNothing res) $ do
        putStrLn $ "WARNING: Could not find command line program " ++ name ++ "."
        when isWindows $ putStrLn $ "  You may be able to install it from:\n  " ++ url
    return res
    where
        srcList = [ ("gzip", "http://gnuwin32.sourceforge.net/packages/gzip.htm")
                  , ("tar",  "http://gnuwin32.sourceforge.net/packages/gtar.htm")
                  , ("wget", "http://gnuwin32.sourceforge.net/packages/wget.htm")
                  , ("curl", "http://curl.haxx.se/download.html")
                  ]
        url = fromJust . lookup name $ srcList

extractTarball :: FilePath -> IO ()
extractTarball out = do
        createDirectoryIfMissing True out
        withDirectory out $ do
            hasGzip <- check "gzip"
            hasTar  <- check "tar"
            when (any isNothing [hasGzip, hasTar]) $ error "Could not extract tarball(s), could not find either gzip or tar on the $PATH."
            putStrLn "Extracting tarball... "
            system_ $ "gzip --decompress --stdout --force .." </> takeFileName out <.> "tar.gz > .." </> takeFileName out <.> "tar"
            system_ $ "tar -xf .." </> takeFileName out <.> "tar"
            putStrLn "Finished extracting tarball"
        writeFile (out <.> "txt") ""
