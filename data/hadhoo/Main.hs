
module Main where

import System.Environment
import System.Cmd
import Control.Monad
import Data.List


helpMsg = "help msg for hadhoo\nhadhoo directory -output.hoo"

main :: IO ()
main = do args <- getArgs
          if null args then putStrLn helpMsg else do
            let (outfiles,infiles) = partition ("-" `isPrefixOf`) args
                outfile = last ("hoogle.txt":map tail outfiles)
            allfiles <- liftM concat $ mapM pickFiles infiles
            if null allfiles
              then putStrLn "No in files specified, nothing to do"
              else execute allfiles outfile


pickFiles :: FilePath -> IO [FilePath]
pickFiles file = return [file]


execute :: [FilePath] -> FilePath -> IO ()
execute srcfiles outfile = mapM_ (executeOne outfile) srcfiles

executeOne :: FilePath -> FilePath -> IO ()
executeOne outfile file = do
    system $ "haddock -hoogle " ++ file ++ " > temp.txt"
    src <- readFile "temp.txt"
    appendFile outfile src


