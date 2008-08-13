
module Hoogle(hoogle) where

import Util

hoogle :: String -> IO ()
hoogle "keyword" = copyFile "temp/keyword/hoogle.txt" "result/keyword.txt"

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
                                              ["@depends " ++ d | d <- depends, d /= "rts"]
            | "@version" `isPrefixOf` x && version /= "" = []
            | otherwise = [x]


cabalInfo src = (version,depends)
    where
        src2 = lines src
        version = head $ readFields "version" src2 ++ [""]
        depends = nub $ filter f $ words $ map (rep ',' ' ') $ unwords $ readFields "build-depends" src2
        f x = x /= "" && isAlpha (head x)


readFields :: String -> [String] -> [String]
readFields name = f
    where
        f (x:xs) | (name ++ ":") `isPrefixOf` map toLower x2 =
                [x4 | x4 /= []] ++ map trim ys ++ f zs
            where
                x4 = trim x3
                x3 = drop (length name + 1) x2
                (spc,x2) = span isSpace x
                (ys,zs) = span ((> length spc) . length . takeWhile isSpace) xs
        f (x:xs) = f xs
        f [] = []


trim = reverse . ltrim . reverse . ltrim
ltrim = dropWhile isSpace
