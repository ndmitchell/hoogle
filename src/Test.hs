
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
    res <- system $ normalise (head (found ++ ["hoogle"])) ++ " test"
    exitWith res
