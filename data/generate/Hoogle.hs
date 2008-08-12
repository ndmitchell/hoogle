
module Hoogle(hoogle) where

import Util

hoogle :: String -> IO ()
hoogle x = do
    -- read the cabal info
    cabal <- readFile $ "temp/" ++ x ++ "/" ++ x ++ ".cabal"
    let (version,depends) = cabalInfo cabal

    -- rewrite with extra information
    src <- readFile $ "temp/" ++ x ++ "/hoogle.txt"
    writeFile ("result/" ++ x ++ ".txt") $ unlines $ concatMap (f version depends)
        $ lines $ filter (/= '\r') src
        -- '\r' because of haddock/cabal interactions going weird..
    where
        f version depends x
            | "@package" `isPrefixOf` x = x : ["@version " ++ version | version /= ""] ++
                                              ["@depends " ++ d | d <- depends]
            | "@version" `isPrefixOf` x && version /= "" = []
            | otherwise = [x]


cabalInfo src = (version,depends)
    where
        src2 = lines src
        version = head $ readFields "version" src2 ++ [""]
        depends = nub $ filter f $ words $ map (rep ',' ' ') $ unwords $ readFields "build-depends" src2
        f x = x /= "" && isAlpha (head x)


readFields :: String -> [String] -> [String]
readFields name = concatMap f
    where
        f x | (name ++ ":") `isPrefixOf` map toLower x2 = [x4]
            where
                x4 = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace x3
                x3 = drop (length name + 1) x2
                x2 = dropWhile isSpace x
        f x = []
