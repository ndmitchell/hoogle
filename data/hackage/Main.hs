{-
    * downloaded/ is a cache of downloaded things to speed bits up
    * haddock/ is a cache of the .txt files and .dep files
    * hoogle/ is the .hoo databases
    * temp/ is very temporary stuff
-}

module Main(main) where

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as Map
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.IO


main = do
    createDirectoryIfMissing True "temp"
    createDirectoryIfMissing True "haddock"
    print "Downloading index"
    file <- wget "hackage.haskell.org/packages/archive/log"
    str <- readFile file
    let pkg = readPackages str
    mapM_ (uncurry readPackage) $ Map.toList pkg


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


evil = ["Emping" -- Has Aux.hs in the tar file
       ,"GPLib" -- been deleted since
       ]

readPackage :: String -> String -> IO ()
readPackage name ver = do
    let url = "hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ ver ++ "/" ++ name ++ "-" ++ ver ++ ".tar.gz"
        hoo = "temp/" ++ name ++ "-" ++ ver ++ "/dist/doc/html/" ++ name ++ "/" ++ name ++ ".txt"
        res = "haddock/" ++ name ++ "-" ++ ver ++ ".txt"
        cabal = "temp/" ++ name ++ "-" ++ ver ++ "/" ++ name ++ ".cabal"

    b1 <- doesFileExist res
    b2 <- doesFileExist (res <.> "fail")
    when (not $ b1 || b2 || name `elem` evil) $ do
        file <- wget url
        system_ $ "tar -xzf " ++ file ++ " -C temp"
        src <- readFile cabal
        let (deps,src2) = filterBuildDepends src
        withDirectory ("temp/" ++ name ++ "-" ++ ver) $ do
            system $ "cabal install"
            system $ "cabal haddock --hoogle"
        b <- doesFileExist hoo
        if b then do
            copyFile hoo res
            putStrLn $ "Success for " ++ name
         else do
            writeFile (res <.> "fail") "Failed :-("
            putStrLn $ "Failure for " ++ name


system_ x = do
    print x
    res <- system x
    when (res /= ExitSuccess) $
        error $ "Command failed, " ++ x


withDirectory new act = do
    old <- getCurrentDirectory
    bracket_ (setCurrentDirectory new) (setCurrentDirectory old) act




filterBuildDepends :: String -> ([String],String)
filterBuildDepends = (nub . concat *** unlines) . unzip . f . lines
    where
        f :: [String] -> [([String],String)]
        f [] = []
        f (x:xs) | name2 == "build-depends" =
                (pkgList rest2, replicate spc ' ' ++ "build-depends:") : f rest
            where
                (spc,text) = (length *** id) $ span isSpace x
                (follow,rest) = span ((> spc) . length . takeWhile isSpace) xs
                (name,rest2) = break (== ':') $ unwords $ text:follow
                name2 = map toLower $ reverse $ dropWhile isSpace $ reverse name
        f (x:xs) = ([],x) : f xs


pkgList x = error $ show ("read the package list",x)



readFile' x = do
    h <- openFile x ReadMode
    src <- hGetContents h
    length src `seq` hClose h
    return src
