
module Main(main) where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath


main = do
    print "Downloading index"
    file <- wget "hackage.haskell.org/packages/archive/log"
    str <- readFile file
    let pkg = readPackages str
    print pkg


wget :: String -> IO FilePath
wget url = do
    let file = "downloaded" </> url
    createDirectoryIfMissing True (dropFileName file)
    b <- doesFileExist file
    when (not b) $ do
        putStrLn $ "Downloading " ++ url
        r <- system $ "wget -q http://" ++ url ++ " -O " ++ file
        when (r /= ExitSuccess) $ do
            removeFile file
            error "Downloading failed"
    return file



readPackages :: String -> Map.Map String String
readPackages = foldl' f Map.empty . map words . lines
    where f mp ws = Map.insert (ws !! 7) (ws !! 8) mp
