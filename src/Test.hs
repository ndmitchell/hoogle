module Main(main) where

import Control.Monad
import System.Process
import System.Directory
import System.Exit
import System.FilePath
import System.Environment
import System.IO.Temp


main :: IO ()
main = do
    let files = ["./dist/build/hoogle/hoogle.exe","./dist/build/hoogle/hoogle"
                ,"./hoogle.exe","./hoogle"
                ,"../hoogle/hoogle.exe","../hoogle/hoogle"]
    found <- filterM doesFileExist files
    let hoogle args want_success = do
            let cmd = normalise (head (found ++ ["hoogle"])) ++ " " ++ args
            res <- system cmd
            when (res /= ExitSuccess && want_success) $
                 error $ "Command: " ++ cmd ++ "\nFailed with: " ++ show res
            when (res == ExitSuccess && not want_success) $
                 error $ "Command: " ++ cmd ++ "\nExpected failure but didn't fail."

    args <- getArgs
    if "--no-net" `elem` args then
        hoogle "test" True
     else do
        hoogle "data" True
        hoogle "test --example" True

    -- Check --no-download functionality in an empty data directory.
    withSystemTempDirectory "hoogle-no-download.test." $ \tempdir -> do
         hoogle ("data --no-download -d" ++ tempdir) False
