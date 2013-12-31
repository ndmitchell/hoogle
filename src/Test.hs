
module Main(main) where

import Control.Monad
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath


main :: IO ()
main = do
    let files = ["./dist/build/hoogle/hoogle.exe","./dist/build/hoogle/hoogle"
                ,"./hoogle.exe","./hoogle"
                ,"../hoogle/hoogle.exe","../hoogle/hoogle"]
    found <- filterM doesFileExist files
    let hoogle args = do
            let cmd = normalise (head (found ++ ["hoogle"])) ++ " " ++ args
            res <- system cmd
            when (res /= ExitSuccess) $ error $ "Command: " ++ cmd ++ "\nFailed with: " ++ show res

    args <- getArgs
    if "--no-net" `elem` args then
        hoogle "test"
     else do
        hoogle "data"
        hoogle "test --example"
