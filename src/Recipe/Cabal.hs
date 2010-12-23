
module Recipe.Cabal(
    Cabal(..), readCabal, readCabalDepends, readCabalField
    ) where

import General.Base
import General.Util


data Cabal = Cabal {cabalDepends :: [String], cabalDescription :: [String]}

readCabal :: FilePath -> IO Cabal
readCabal file = do
    src <- fmap lines $ readFile' file
    return $ Cabal (readCabalDepends src) (readCabalField src True "description")


readCabalDepends :: [String] -> [String]
readCabalDepends xs = nub $ map (takeWhile g) $ filter f $ words $ map (rep ',' ' ') $ unwords $ readCabalField xs False "build-depends"
    where f x = x /= "" && isAlpha (head x)
          g x = isAlphaNum x || x `elem` "-_"


readCabalField :: [String] -> Bool -> String -> [String]
readCabalField xs root name = f xs
    where
        f (x:xs) | (name ++ ":") `isPrefixOf` map toLower x2 && (null spc || not root) =
                [x4 | x4 /= []] ++ map (rep "." "" . trim) ys ++ f zs
            where
                x4 = trim x3
                x3 = drop (length name + 1) x2
                (spc,x2) = span isSpace x
                (ys,zs) = span ((> length spc) . length . takeWhile isSpace) xs
        f (x:xs) = f xs
        f [] = []
