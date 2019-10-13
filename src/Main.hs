
module Main(main) where

import System.Environment
import System.IO
import Hoogle


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hoogle =<< getArgs
