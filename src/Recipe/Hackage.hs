{-# LANGUAGE RecordWildCards #-}
module Recipe.Hackage(hackage, platform) where

import Recipe.Type


hackage :: RecipeDetails -> String -> IO ()
hackage RecipeDetails{..} _ = do
    download "hackage-index.web" "http://hackage.haskell.org/cgi-bin/hackage-scripts/search"
    pkgs <- readHackageIndex
    parallel_ $ flip map pkgs $ \(name,ver) -> do
        let url = "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ ver
        b <- tryDownload (name ++ "-haddock.web") $ url ++ "/doc/html/" ++ name ++ ".txt"
        if not b then putStrLn $ "Warning: Could not download Hackage database for " ++ name else do
            download (name ++ "-cabal.web") $ url ++ "/" ++ name ++ ".cabal"
            

readHackageIndex :: IO [(String,String)]
readHackageIndex = return [("uniplate","1.5.1")
                          ,("haskell-src","1.0.1.3")]



platform :: RecipeDetails -> String -> IO ()
platform RecipeDetails{..} _ = do
    download "platform-cabal.web" "http://code.haskell.org/haskell-platform/haskell-platform.cabal"
    pkgs <- readPlatformIndex
    print pkgs


readPlatformIndex :: IO [String]
readPlatformIndex = return ["haskell-src"]
