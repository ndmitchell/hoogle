
module Haddock(haddock) where

import Util


haddock :: String -> IO ()
haddock "keywords" = error "haddock keywords"


haddock x = do
    let res = "temp/" ++ x ++ "/hoogle.txt"
    b <- doesFileExist res
    when (not b) $ do
        when (x == "base") $ basePatchup

        b <- doesFileExist $ "temp/" ++ x ++ "/setup.exe"
        when (not b) $ setupFile ("temp/" ++ x ++ "/setup.exe")

        dir <- getCurrentDirectory
        bracket_ (setCurrentDirectory $ "temp/" ++ x) (setCurrentDirectory dir) $ do
            system_ "setup configure"
            system_ "setup haddock --hoogle"

        copyFile ("temp/" ++ x ++ "/dist/doc/html/" ++ x ++ "/" ++ x ++ ".txt") res


basePatchup = do
    -- FIX THE CABAL FILE
    let cabal = "temp/base/base.cabal"
    x <- readFile' cabal
    let f x = not $ "ghc.prim" `isSubstrOf` map toLower x
    x <- return $ unlines $ filter f $ lines x
    writeBinaryFile cabal x

    -- INCLUDE FILE
    copyFile "Config.h" "temp/base/include/HsBaseConfig.h"


setupFile file = do
    b <- doesFileExist "temp/setup.exe"
    when (not b) $ do
        writeFile "temp/Setup.hs" "import Distribution.Simple; main = defaultMain"
        system_ "ghc --make temp/Setup.hs -o temp/setup.exe"
    copyFile "temp/setup.exe" file
