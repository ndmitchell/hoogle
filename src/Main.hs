
module Main where

import CmdLine.All
import Web.All


main :: IO ()
main = do
    q <- cmdQuery
    if queryWeb q
        then actionWeb q
        else actionCmdLine q
