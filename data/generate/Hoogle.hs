
module Hoogle(hoogle) where

import Util

hoogle :: String -> IO ()
hoogle "keyword" = copyFile "temp/keyword/hoogle.txt" "result/keyword.txt"

hoogle name = do
    -- read the cabal info
    cabal <- liftM lines $ readFile $ "temp/" ++ name ++ "/" ++ name ++ ".cabal"

    -- rewrite with extra information
    src <- readFile $ "temp/" ++ name ++ "/hoogle.txt"
    writeFile ("result/" ++ name ++ ".txt") $ unlines $ concatMap (f cabal)
        $ lines $ filter (/= '\r') src
        -- '\r' because of haddock/cabal interactions going weird..
    where
        f cabal x
            | "@package" `isPrefixOf` x =
                [x] ++
                ["@version " ++ if name == "base" then "latest" else v
                    | let v = cabalVersion cabal, v /= ""] ++
                ["@depends " ++ d | d <- cabalDepends cabal, d /= "rts"]
            | "@version" `isPrefixOf` x = []
            | otherwise = [x]


cabalVersion xs = head $ readFields "version" xs ++ [""]

cabalDepends xs = nub $ filter f $ words $ map (rep ',' ' ') $ unwords $ readFields "build-depends" xs
    where f x = x /= "" && isAlpha (head x)


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
