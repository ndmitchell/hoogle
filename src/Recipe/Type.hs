{-# LANGUAGE RecordWildCards #-}

module Recipe.Type(
    RecipeOptions(..),
    RecipeDetails(..), recipeDetails,
    ls,
    Cabal(..), readCabal
    ) where

import General.Code
import General.Parallel
import Hoogle
import Data.Monoid


data RecipeOptions = RecipeOptions
    {recipeDir :: FilePath -- ^ Directory to use
    ,recipeThreads :: Int -- ^ Number of threads to use
    ,recipeNodownload :: Bool -- ^ Don't download anything
    ,recipeRedownload :: Bool -- ^ Download everything from the web
    ,recipeRebuild :: Bool -- ^ Rebuild all local files
    }


data RecipeDetails = RecipeDetails
    {recipeOptions :: RecipeOptions
    ,download :: FilePath -> URL -> IO ()
    ,combine :: FilePath -> [FilePath] -> IO ()
    ,tryDownload :: FilePath -> URL -> IO Bool
    ,process :: [FilePath] -> [FilePath] -> IO () -> IO ()
    ,par :: [IO ()] -> IO ()
    }

    
recipeDetails :: RecipeOptions -> RecipeDetails
recipeDetails recipeOptions@RecipeOptions{..} = RecipeDetails{..}
    where
        par = parallelN_ recipeThreads

        combine to from = process from [to] $ do
            putStrLn $ "Combining " ++ show (length from) ++ " databases"
            xs <- mapM loadDatabase from
            saveDatabase to $ mconcat xs

        tryDownload to url = do
            exists <- doesFileExist to
            if exists && not recipeRedownload then return True
             else if recipeNodownload then return False else do
                res <- system $ "wget " ++ url ++ " -O " ++ to
                let b = res == ExitSuccess
                unless b $ removeFile to
                return b

        download to url = do
            b <- tryDownload to url
            unless b $ error $ "Failed to download " ++ url

        process from to act = do
            exists <- fmap and $ mapM doesFileExist to
            rebuild <- if not exists then return True else do
                old <- fmap maximum $ mapM getModificationTime from
                new <- fmap minimum $ mapM getModificationTime to
                return $ old >= new
            when (rebuild || recipeRebuild) act


ls :: (FilePath -> Bool) -> IO [FilePath]
ls f = do
    xs <- getDirectoryContents "."
    return $ sort $ filter f xs



data Cabal = Cabal {cabalDepends :: [String]}

readCabal :: FilePath -> IO Cabal
readCabal file = do
    src <- readFile file
    return $ Cabal $ readCabalDepends $ lines src


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
