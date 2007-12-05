
module Main where

import System.Cmd
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad


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
    system_ $ "ghc -i --make Setup"
    system_ $ "setup configure"
    system_ $ "setup haddock --hoogle -v"

    setCurrentDirectory "../.."


system_ x = do
    putStrLn $ "Running: " ++ x
    res <- system x
    when (res /= ExitSuccess) $
        error "Command failed"
