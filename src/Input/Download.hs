{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Download(downloadInputs) where

import General.Web
import General.Util
import System.FilePath
import Control.Monad.Extra
import System.Directory


urls =
    [("stackage.txt","http://www.stackage.org/lts/cabal.config")
    ,("platform.txt","https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/src/Releases2014.hs")
    ,("cabal.tar.gz","http://hackage.haskell.org/packages/index.tar.gz")
    ,("hoogle.tar.gz","http://hackage.haskell.org/packages/hoogle.tar.gz")
    ,("reverse.htm","http://packdeps.haskellers.com/reverse")
    ]

downloadInputs :: IO ()
downloadInputs = do
    createDirectoryIfMissing True "input"
    forM_ urls $ \(name, url) -> do
        let file = "input" </> name
        unlessM (doesFileExist file) $ do
            timed ("Downloading " ++ url) $ do
                downloadFile (file <.> "part") url
                renameFile (file <.> "part") file
