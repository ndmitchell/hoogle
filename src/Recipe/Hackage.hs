{-# LANGUAGE RecordWildCards #-}
module Recipe.Hackage(package, platform, hackage) where

import Recipe.Type
import General.Code


package :: RecipeDetails -> [String] -> IO ()
package r@RecipeDetails{..} args = do
    pkgs <- if null args then readHackage r else do
        let ps = map parsePackage args
        if all (not . null . snd) ps then return ps else do
            h <- readHackage r
            return [(a, if null b then fromMaybe (error $ "No version for " ++ a) $ lookup a h else b) | (a,b) <- ps]
    par $ flip map pkgs $ \(name,ver) -> do
        let url = "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ ver
            had = name ++ "-" ++ ver ++ "-haddock.web"
            cab = name ++ "-" ++ ver ++ "-cabal.web"
        b <- tryDownload had $ url ++ "/doc/html/" ++ name ++ ".txt"
        if not b then putStrLn $ "Warning: Could not download Hackage database for " ++ name else do
            download cab $ url ++ "/" ++ name ++ ".cabal"
            process [cab,had] [name ++ ".txt"] $ packageTextbase (name,ver)


packageTextbase :: (String,String) -> IO ()
packageTextbase (name,ver) = do
    src <- readFile $ name ++ "-" ++ ver ++ "-haddock.web"
    writeFile (name ++ ".txt") $ haddockHacks src


hackage :: RecipeDetails -> [String]  -> IO ()
hackage r@RecipeDetails{..} _ = do
    xs <- readHackage r
    writeFile "hackage.txt" $ unlines $ concat
        [["","-- | " ++ ver,"@package " ++ name] | (name,ver) <- xs]


parsePackage :: String -> (String,String)
parsePackage = second (drop 1) . rbreak (== '-')


readHackage :: RecipeDetails -> IO [(String,String)]
readHackage RecipeDetails{..} = do
    download "-hackage.web" $ "http://hackage.haskell.org/cgi-bin/hackage-scripts/search " ++
                              "--post-data=search=has-library:yes"
    src <- readFile "-hackage.web"
    let pre = "><a href=\"/package/"
    return $ map (parsePackage . init . drop (length pre)) $
            filter (isPrefixOf pre) $ map trim $ lines src


platform :: RecipeDetails -> [String] -> IO ()
platform r@RecipeDetails{..} _ = do
    pkgs <- readPlatform r
    combine "platform.hoo" [a <.> "hoo" | (a,b) <- pkgs]


readPlatform :: RecipeDetails -> IO [(String,String)]
readPlatform RecipeDetails{..} = do
    download "-platform.web" "http://code.haskell.org/haskell-platform/haskell-platform.cabal"
    src <- readFile "-platform.web"
    let xs = takeWhile (not . isPrefixOf "build-tools:" . ltrim) $
             dropWhile (not . isPrefixOf "build-depends:" . ltrim) $
             lines src
    return [(name, takeWhile (\x -> x == '.' || isDigit x) $ drop 1 b)
           | x <- xs, (a,_:b) <- [break (== '=') x], let name = trim $ dropWhile (== '-') $ trim a
           , name `notElem` words "Cabal hpc Win32"]


---------------------------------------------------------------------
-- HADDOCK HACKS

-- Eliminate @version
-- Change :*: to (:*:), Haddock bug
-- Change !!Int to !Int, Haddock bug
-- Change instance [overlap ok] to instance, Haddock bug
-- Change instance [incoherent] to instance, Haddock bug
-- Change !Int to Int, HSE bug

haddockHacks :: String -> String
haddockHacks = unlines . map (unwords . map f . words) . filter (not . isPrefixOf "@version ") . lines
    where
        f "::" = "::"
        f (':':xs) = "(:" ++ xs ++ ")"
        f ('!':'!':x:xs) | isAlpha x = xs
        f ('!':x:xs) | isAlpha x || x `elem` "[(" = xs
        f x | x `elem` ["[overlap","ok]","[incoherent]"] = ""
        f x = x
