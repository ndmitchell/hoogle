{-# LANGUAGE RecordWildCards #-}

module Recipe.Command(wget, ungzip, tarExtract, tarList) where

import General.Base
import General.System
import Development.Shake
import Development.Shake.FilePath
import CmdLine.All as C


wget :: C.CmdLine -> URL -> FilePath -> Action ()
wget opt@Data{..} from to = do
    when nodownload $
         error "Downloads are disabled."
    dl <- liftIO findDownloader
    command [Shell] (dl to from) []


ungzip :: FilePath -> FilePath -> Action ()
ungzip from to = do
    hasGzip <- liftIO $ check "gzip"
    when (isNothing hasGzip) $ error "Could not extract tarballs, could not find tar on the $PATH."
    command [Shell] ("gzip --decompress --stdout --force " ++ from ++ " > " ++ to) []

tarExtract :: FilePath -> Action ()
tarExtract from = do
    hasTar  <- liftIO $ check "tar"
    when (isNothing hasTar) $ error "Could not extract tarballs, could not find tar on the $PATH."
    liftIO $ createDirectoryIfMissing True $ dropExtension from
    command [Shell, Cwd $ dropExtension from] ("tar -xf ../" ++ takeFileName from) []

tarList :: FilePath -> Action [String]
tarList from = do
    hasTar  <- liftIO $ check "tar"
    when (isNothing hasTar) $ error "Could not extract tarballs, could not find tar on the $PATH."
    fmap (lines . fromStdout) $ command [Shell] ("tar -tf " ++ from) []


type Downloader = FilePath -> URL -> String

wget2 :: Downloader
wget2 fp url = "wget -nv --no-check-certificate " ++ url ++ " --output-document=" ++ fp
curl :: Downloader
curl fp url = "curl -sSL " ++ url ++ " --output " ++ fp

findDownloader :: IO Downloader
findDownloader = do
    dl <- check "wget"
    dl <- maybe (check "curl") (return . Just) dl
    when (isNothing dl) $ error "Could not find downloader, neither curl nor wget are on the $PATH."
    return $ matchDl (fromJust dl)
    where matchDl d | "wget" `isInfixOf` d = wget2
                    | "curl" `isInfixOf` d = curl


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
