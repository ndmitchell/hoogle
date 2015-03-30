{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Download(downloadInputs) where

import General.Util
import System.FilePath
import Control.Monad.Extra
import System.Directory
import Data.Conduit.Binary (sinkFile)
import qualified Network.HTTP.Conduit as C
import qualified Data.Conduit as C
import Network


urls =
    [("stackage.txt","https://www.stackage.org/lts/cabal.config")
    ,("platform.txt","https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/src/Releases2014.hs")
    ,("cabal.tar.gz","https://hackage.haskell.org/packages/index.tar.gz")
    ,("hoogle.tar.gz","https://hackage.haskell.org/packages/hoogle.tar.gz")
    ,("reverse.htm","https://packdeps.haskellers.com/reverse")
    ]

-- | Download all the input files to input/
downloadInputs :: IO ()
downloadInputs = do
    createDirectoryIfMissing True "input"
    forM_ urls $ \(name, url) -> do
        let file = "input" </> name
        unlessM (doesFileExist file) $ do
            timed ("Downloading " ++ url) $ do
                downloadFile (file <.> "part") url
                renameFile (file <.> "part") file

downloadFile :: FilePath -> String -> IO ()
downloadFile file url = withSocketsDo $ do
    request <- C.parseUrl url
    C.withManager $ \manager -> do
        response <- C.http request manager
        C.responseBody response C.$$+- sinkFile file
