
module Hoogle(hoogle) where

import Util

hoogle :: String -> IO ()
hoogle name = do
    -- read the cabal info
    cabal <- readCabal $ "temp/" ++ name ++ "/" ++ name ++ ".cabal"
    let version = cabalVersion cabal
        prefix = ["@package " ++ name
                 ,"@version " ++ version
                 ,"@haddock http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ version ++ "/doc/html/"
                 ,"@hackage http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ name] ++
                 ["@depends " ++ d | d <- cabalDepends cabal]

    -- rewrite with extra information
    src <- readTextBase $ "temp/" ++ name ++ "/hoogle.txt"
    writeFile ("result/" ++ name ++ ".txt") $ unlines $ replaceTextBasePrefix prefix src
