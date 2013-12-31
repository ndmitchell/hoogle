
module Recipe.Hackage(platformPackages, avoid, findLocal) where

import General.Base
import General.System
import General.Util
import General.Web
import System.FilePath hiding (combine)


-- FIXME: This is a list of hack
avoid = words "ghc-prim integer integer-simple integer-gmp rts ghc Win32"


-- try and find a local filepath
findLocal :: [FilePath] -> String -> IO (Maybe URL)
findLocal paths name = fmap (listToMaybe . concat . concat) $ forM paths $ \p -> do
    xs <- getDirectoryContents p
    xs <- return [p </> x | x <- reverse $ sort xs, name == fst (rbreak (== '-') x)] -- make sure highest version comes first
    forM xs $ \x -> do
        b <- doesDirectoryExist $ x </> "html"
        x <- return $ if b then x </> "html" else x
        b <- doesFileExist $ x </> "doc-index.html"
        return [filePathToURL $ x </> "index.html" | b]


---------------------------------------------------------------------
-- READ PLATFORM

platformPackages :: String -> [String]
platformPackages = map fst . parsePlatform

parsePlatform src =
    let xs = takeWhile (not . isPrefixOf "build-tools:" . ltrim) $
             dropWhile (not . isPrefixOf "build-depends:" . ltrim) $
             lines src
    in [(name, takeWhile (\x -> x == '.' || isDigit x) $ drop 1 b)
       | x <- xs, (a,_:b) <- [break (== '=') x], let name = trim $ dropWhile (== '-') $ trim a
       , not $ avoid name]
    where
        avoid x = ("haskell" `isPrefixOf` x && all isDigit (drop 7 x)) ||
                  (x `elem` words "Cabal hpc Win32")
