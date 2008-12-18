
module Main where

import ScrapeModule

main = do
    src <- readFile "XMonad-Core.html"
    putStr $ unlines $ scrapeModule src
