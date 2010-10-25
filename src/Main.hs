
module Main where

import CmdLine.All
import Console.All as Console
import Web.All as Web


main :: IO ()
main = do
    q <- cmdLine
    if isWebCmdLine q then Web.action q else Console.action q
