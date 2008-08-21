
module Download(download) where

import Util
import Text.HTML.TagSoup
import Text.HTML.Download


download :: String -> IO ()
download x = do
    depends ("temp/" ++ x ++ "/" ++ x ++ ".cabal") [] $ do
        src <- openURL $ "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ x
        let link = head [url | TagOpen "a" [("href",url)] <- parseTags src, ".tar.gz" `isSuffixOf` url]
        system_ $ "wget http://hackage.haskell.org/" ++ link ++ " -O temp/" ++ x ++ ".tar.gz"
        system_ $ "gunzip --force temp/" ++ x ++ ".tar.gz"
        system_ $ "tar -xf temp/" ++ x ++ ".tar -C temp"
        renameDirectory ("temp/" ++ dropExtension (takeBaseName link)) ("temp/" ++ x)

