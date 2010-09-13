
module Main where

import CmdLine.All
import Console.Action as Console
import Web.Action as Web


main :: IO ()
main = do
    q <- cmdQuery
    if queryWeb q
        then Web.action q
        else Console.action q
