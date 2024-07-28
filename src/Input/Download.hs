{-# LANGUAGE TupleSections #-}

module Input.Download(downloadInput, DownloadInput(..)) where

import System.FilePath
import Control.Monad.Extra
import System.Directory
import Data.Conduit.Binary (sinkFile)
import Data.Default.Class
import qualified Network.HTTP.Conduit as C
import Network.Connection
import qualified Data.Conduit as C
import General.Util
import General.Timing
import Control.Monad.Trans.Resource
import Control.Exception.Extra

data DownloadInput =
    AlwaysDownloadInput
    | NeverDownloadInput
    | DownloadInputIfNotThere


-- | Download all the input files to input/
downloadInput :: Timing -> Bool -> DownloadInput -> FilePath -> String -> URL -> IO FilePath
downloadInput timing insecure download dir name url = do
    let file = dir </> "input-" ++ name
    exists <- doesFileExist file
    let act =
            timed timing ("Downloading " ++ url) $ do
                downloadFile insecure (file <.> "part") url
                renameFile (file <.> "part") file
    case download of
        NeverDownloadInput ->
            unless exists $
              errorIO $ "File is not already downloaded and --download=no given, downloading " ++ url ++ " to " ++ file
        AlwaysDownloadInput -> act
        DownloadInputIfNotThere ->
            unless exists act
    pure file

downloadFile :: Bool -> FilePath -> String -> IO ()
downloadFile insecure file url = do
    let request = C.parseRequest_ url
    manager <- C.newManager $ C.mkManagerSettings
      (TLSSettingsSimple {
        settingDisableCertificateValidation = insecure,
        settingDisableSession = False,
        settingUseServerName = False,
        settingClientSupported = def
      }) Nothing
    runResourceT $ do
        response <- C.http request manager
        C.runConduit $ C.responseBody response C..| sinkFile file
