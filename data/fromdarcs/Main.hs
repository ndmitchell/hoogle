
module Main where

import System.Cmd
import System.Exit
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Data.List


packages = ["http://darcs.haskell.org/packages/base/"
           ,"http://darcs.haskell.org/packages/filepath/"
           ]

main = do
    createDirectoryIfMissing True "grab"
    mapM_ generate packages


generate url = do
    let name = last $ words $ map (\x -> if x == '/' then ' ' else x) url
        dir = "grab" </> name
        exe = "grab" </> name </> "Setup"
    b <- doesDirectoryExist dir

    if b
        then system_ $ "darcs pull --repodir=" ++ dir
        else system_ $ "darcs get --partial " ++ url ++ " --repodir=" ++ dir

    setCurrentDirectory $ "grab" </> name
    removeFile_ "Setup.hs"
    removeFile_ "Setup.lhs"
    writeFile "Setup.hs" "import Distribution.Simple; main = defaultMain"
    when (name == "base") $ fixupBaseCabal
    system_ $ "ghc -i --make Setup"
    system_ $ "setup configure"
    system_ $ "setup haddock --hoogle"

    setCurrentDirectory "../.."


system_ x = do
    putStrLn $ "Running: " ++ x
    res <- system x
    when (res /= ExitSuccess) $
        error "Command failed"

removeFile_ x = do
    b <- doesFileExist x
    when b $ removeFile x

readFile' x = do
    h <- openFile x ReadMode
    s <- hGetContents h
    () <- length s `seq` return ()
    hClose h
    return s

fixupBaseCabal = do
    x <- readFile' "base.cabal"
    let (pre,post) = break ("impl(ghc)" `isInfixOf`) $ lines x
        post2 = drop 1 $ dropWhile ('}' `notElem`) post
    writeFile "base.cabal" $ unlines $ pre ++ post2
