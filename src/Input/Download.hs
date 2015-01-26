{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Download(downloadInputs) where

import General.Web
import System.FilePath
import Control.Monad.Extra
import System.Directory
import System.IO


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
            putStr $ "Downloading " ++ url ++ "... "
            hFlush stdout
            downloadFile (file <.> "part") url
            renameFile (file <.> "part") file
            putStrLn "done"
