{-# LANGUAGE RecordWildCards #-}
module Recipe.Hackage(package, platform, hackage) where

import Recipe.Type
import General.Code


package :: RecipeDetails -> String -> IO ()
package RecipeDetails{..} _ = do
    download "-hackage.web" "http://hackage.haskell.org/cgi-bin/hackage-scripts/search"
    pkgs <- readHackage
    parallel_ $ flip map pkgs $ \(name,ver) -> do
        let url = "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ ver
        b <- tryDownload (name ++ "-haddock.web") $ url ++ "/doc/html/" ++ name ++ ".txt"
        if not b then putStrLn $ "Warning: Could not download Hackage database for " ++ name else do
            download (name ++ "-cabal.web") $ url ++ "/" ++ name ++ ".cabal"
            process [name ++ "-cabal.web", name ++ "-haddock.web"] [name ++ ".txt"] $ packageTextbase name


packageTextbase :: String -> IO ()
packageTextbase name = copyFile (name ++ "-haddock.web") (name ++ ".txt")



hackage :: RecipeDetails -> String -> IO ()
hackage RecipeDetails{..} _ = do
    pkgs <- readHackage
    combine "hackage.hoo" [a <.> "hoo" | (a,b) <- pkgs]


readHackage :: IO [(String,String)]
readHackage = return [("uniplate","1.5.1")
                     ,("haskell-src","1.0.1.3")]


platform :: RecipeDetails -> String -> IO ()
platform RecipeDetails{..} _ = do
    download "-platform.web" "http://code.haskell.org/haskell-platform/haskell-platform.cabal"
    pkgs <- readPlatform
    combine "platform.hoo" [a <.> "hoo" | (a,b) <- pkgs]


readPlatform :: IO [(String,String)]
readPlatform = do
    src <- readFile "-platform.web"
    let xs = takeWhile (not . isPrefixOf "build-tools:" . ltrim) $
             dropWhile (not . isPrefixOf "build-depends:" . ltrim) $
             lines src
    return [(trim $ filter (/= '-') a, takeWhile (\x -> x == '.' || isDigit x) $ drop 1 b)
           | x <- xs, (a,_:b) <- [break (== '=') x]]
