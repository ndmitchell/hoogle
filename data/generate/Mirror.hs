
module Mirror(mirror) where

import System.Cmd
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad


-- given a URL, return a pointer to that file
-- assumed "http://hackage.haskell.org/"
-- bool = is the file fresh
mirror :: FilePath -> IO (FilePath,Bool)
mirror url = do
    let file = "temp/mirror/" ++ url
    createDirectoryIfMissing True $ takeDirectory file
    b <- doesFileExist file
    unless b $ do
        res <- system $ "wget http://hackage.haskell.org/" ++ url ++ " -O " ++ file
        when (res /= ExitSuccess) $
            writeFile file ""
    return (file,not b)
