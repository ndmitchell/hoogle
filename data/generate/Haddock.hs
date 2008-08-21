
module Haddock(haddock) where

import Util


haddock :: String -> IO ()
haddock x = do
    let res = "temp/" ++ x ++ "/hoogle.txt"
    depends res [] $ do
        setupFile $ "temp/" ++ x ++ "/setup.exe"

        dir <- getCurrentDirectory
        bracket_ (setCurrentDirectory $ "temp/" ++ x) (setCurrentDirectory dir) $ do
            system_ "setup configure"
            system_ "setup haddock --hoogle"

        copyFile ("temp/" ++ x ++ "/dist/doc/html/" ++ x ++ "/" ++ x ++ ".txt") res


setupFile file = do
    depends file [] $ do
        depends "temp/setup.exe" [] $ do
            writeFile "temp/Setup.hs" "import Distribution.Simple; main = defaultMain"
            system_ "ghc --make temp/Setup.hs -o temp/setup.exe"
        copyFile "temp/setup.exe" file
