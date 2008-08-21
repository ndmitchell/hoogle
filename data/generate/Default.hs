
module Default where

import Util
import Text.HTML.TagSoup
import Text.HTML.Download


processDefault x = do
    download x
    haddock x
    hoogle x


download :: String -> IO ()
download x = do
    depends ("temp/" ++ x ++ "/" ++ x ++ ".cabal") [] $ do
        src <- openURL $ "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ x
        let link = head [url | TagOpen "a" [("href",url)] <- parseTags src, ".tar.gz" `isSuffixOf` url]
        system_ $ "wget http://hackage.haskell.org/" ++ link ++ " -O temp/" ++ x ++ ".tar.gz"
        system_ $ "gunzip --force temp/" ++ x ++ ".tar.gz"
        system_ $ "tar -xf temp/" ++ x ++ ".tar -C temp"
        renameDirectory ("temp/" ++ dropExtension (takeBaseName link)) ("temp/" ++ x)


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


hoogle :: String -> IO ()
hoogle name = do
    -- read the cabal info
    cabal <- readCabal $ "temp/" ++ name ++ "/" ++ name ++ ".cabal"
    let version = cabalVersion cabal
        prefix = ["@package " ++ name
                 ,"@version " ++ version
                 ,"@haddock http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ version ++ "/doc/html/"
                 ,"@hackage http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ name] ++
                 ["@depends " ++ d | d <- cabalDepends cabal]

    -- rewrite with extra information
    src <- readTextBase $ "temp/" ++ name ++ "/hoogle.txt"
    writeFile ("result/" ++ name ++ ".txt") $ unlines $ replaceTextBasePrefix prefix src

