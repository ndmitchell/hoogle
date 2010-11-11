{-# LANGUAGE RecordWildCards #-}
module Recipe.Hackage(package, platform, hackage) where

import Recipe.Type
import General.Code


package :: RecipeDetails -> [String] -> IO ()
package r@RecipeDetails{..} args = do
    pkgs <- if null args then readHackage r else return $ map parsePackage args
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
    writeFile (name ++ ".txt") $ unlines $ filter (not . isPrefixOf "@version ") $ lines src


hackage :: RecipeDetails -> [String]  -> IO ()
hackage r@RecipeDetails{..} _ = do
    pkgs <- readHackage r
    combine "hackage.hoo" [a <.> "hoo" | (a,b) <- pkgs]


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
