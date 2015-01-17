{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Download(downloadInputs) where

import General.Web
import System.FilePath
import Control.Monad.Extra
import System.Directory


urls =
    [("keywords.htm","https://www.haskell.org/haskellwiki/Keywords")
    ,("stackage.txt","http://www.stackage.org/lts/cabal.config")
    ,("platform.txt","https://cdn.rawgit.com/haskell/haskell-platform/496cd9d8d0cfe3b013e6531be19eb836d7599cfe/hptool/src/Releases2014.hs")
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
            downloadFile (file <.> "part") url
            renameFile (file <.> "part") file
            putStrLn "done"
