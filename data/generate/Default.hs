
module Default where

import Util
import Text.HTML.TagSoup


processDefault x = do
    download x
    haddock x
    hoogle x


download :: String -> IO ()
download x = do
    depends ("temp/" ++ x ++ "/" ++ x ++ ".cabal") [] $ do
        src <- readUrl $ "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ x
        let link = head [url | TagOpen "a" [("href",url)] <- parseTags src, ".tar.gz" `isSuffixOf` url]
        system_ $ "wget http://hackage.haskell.org/" ++ link ++ " -O temp/" ++ x ++ ".tar.gz"
        system_ $ "gunzip --force temp/" ++ x ++ ".tar.gz"
        system_ $ "tar -xf temp/" ++ x ++ ".tar -C temp"
        renameDirectory ("temp/" ++ dropExtension (takeBaseName link)) ("temp/" ++ x)


fixupCabal :: String -> (String -> [String]) -> IO ()
fixupCabal name f = do
    let file = "temp/" ++ name ++ "/" ++ name ++ ".cabal"
    x <- readFile' file
    x <- return $ unlines $ "Build-Type: Simple" : concatMap g (lines x)
    writeBinaryFile file x
    where
        g x | "build-type" `isPrefixOf` map toLower x = []
            | otherwise = f x


haddock :: String -> IO ()
haddock x = do
    let res = "temp/" ++ x ++ "/hoogle.txt"
    depends res [] $ do
        fixupCabal x (:[])
        dir <- getCurrentDirectory
        bracket_ (setCurrentDirectory $ "temp/" ++ x) (setCurrentDirectory dir) $ do
            system_ "cabal configure"
            system_ "cabal haddock --hoogle"

        copyFile ("temp/" ++ x ++ "/dist/doc/html/" ++ x ++ "/" ++ x ++ ".txt") res


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

