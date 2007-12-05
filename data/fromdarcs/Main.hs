
module Main where

import System.Cmd
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.IO

import Control.Monad
import Data.List
import Data.Maybe


packages = ["http://darcs.haskell.org/packages/base/"
           ,"http://darcs.haskell.org/packages/filepath/"
           ,"http://darcs.haskell.org/packages/containers/"
           ,"http://darcs.haskell.org/packages/array/"
           ,"http://darcs.haskell.org/packages/directory/"
           ,"http://darcs.haskell.org/packages/parsec/"
           ,"http://darcs.haskell.org/packages/mtl/"
           ,"http://darcs.haskell.org/packages/process/"
           ,"http://darcs.haskell.org/packages/random/"
           ,"http://darcs.haskell.org/packages/old-time/"
           ,"http://darcs.haskell.org/packages/old-locale/"
           ,"http://darcs.haskell.org/packages/network/"
           ,"http://darcs.haskell.org/packages/template-haskell/"
           ,"http://darcs.haskell.org/packages/cabal/"
           ,"http://darcs.haskell.org/packages/time/"
           ,"http://darcs.haskell.org/packages/packedstring/"
           ,"http://darcs.haskell.org/packages/bytestring/"
           ,"http://darcs.haskell.org/packages/stm/"
           ,"http://darcs.haskell.org/packages/parallel/"
           ]

main = do
    args <- getArgs
    let rebuild = "skip" `notElem` args
    createDirectoryIfMissing True "grab"
    xs <- mapM (generate rebuild) packages
    (entires,docs) <- mapAndUnzipM divide xs
    writeFile "hoogle.txt" (unlines $ concat entires)
    writeFile "documentation.txt" (unlines $ concat docs)


divide file = do
    s <- readFile file
    let entries = lines s
        name = takeBaseName file
        docs = [drop 7 i ++ "\t" ++ name | i <- entries, "module " `isPrefixOf` i]
    return (entries, docs)

generate rebuild url = do
    let name = last $ words $ map (\x -> if x == '/' then ' ' else x) url
        dir = "grab" </> name
        exe = "grab" </> name </> "Setup"
    ans <- findDatabase name

    when (rebuild || isNothing ans) $ do
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

    ans <- findDatabase name
    return $ fromJust ans


findDatabase name = do
    let dir = "grab" </> name </> "dist" </> "doc" </> "html" </> name
    b <- doesDirectoryExist dir
    files <- if not b then return [] else getDirectoryContents dir
    return $ listToMaybe $ map (dir </>) $ filter ((==) ".txt" . takeExtension) files


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
