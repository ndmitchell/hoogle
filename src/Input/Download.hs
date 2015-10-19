{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- Use conduitManagerSettings to work with http-conduit-2.1.6 and below

module Input.Download(downloadInputs) where

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
    ,("ghc.txt","http://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/ghc.txt")
    ,("cabal.tar.gz","https://hackage.haskell.org/packages/index.tar.gz")
    ,("hoogle.tar.gz","https://hackage.haskell.org/packages/hoogle.tar.gz")
    ]

-- | Download all the input files to input/
downloadInputs :: (String -> IO () -> IO ()) -> Maybe Bool -> FilePath -> IO ()
downloadInputs timed download dir = do
    forM_ urls $ \(name, url) -> do
        let file = dir </> "input-" ++ name
        exists <- doesFileExist file
        when (not exists && download == Just False) $
            error $ "File is not already downloaded and --download=no given, downloading " ++ url ++ " to " ++ file
        when (not exists || download == Just True) $
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
