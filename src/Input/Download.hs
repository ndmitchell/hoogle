{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- Use conduitManagerSettings to work with http-conduit-2.1.6 and below

module Input.Download(downloadInputs) where

import General.Util
import System.FilePath
import Control.Monad.Extra
import System.Directory
import Data.Conduit.Binary (sinkFile)
import qualified Network.HTTP.Conduit as C
import qualified Data.Conduit as C
import Network
import Control.Monad.Trans.Resource


urls =
    [("stackage.txt","https://www.stackage.org/lts/cabal.config")
    ,("platform.txt","https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/src/Releases2015.hs")
    ,("cabal.tar.gz","https://hackage.haskell.org/packages/index.tar.gz")
    ,("hoogle.tar.gz","https://hackage.haskell.org/packages/hoogle.tar.gz")
    ]

-- | Download all the input files to input/
downloadInputs :: FilePath -> IO ()
downloadInputs dir = do
    forM_ urls $ \(name, url) -> do
        let file = dir </> "input-" ++ name
        unlessM (doesFileExist file) $ do
            timed ("Downloading " ++ url) $ do
                downloadFile (file <.> "part") url
                renameFile (file <.> "part") file

downloadFile :: FilePath -> String -> IO ()
downloadFile file url = withSocketsDo $ do
    request <- C.parseUrl url
    manager <- C.newManager C.conduitManagerSettings
    runResourceT $ do
        response <- C.http request manager
        C.responseBody response C.$$+- sinkFile file
