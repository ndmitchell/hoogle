
module Main(main) where

import System.Cmd
import System.Exit

main :: IO ()
main = exitWith =<< system "hoogle test"
