{-# LANGUAGE RecordWildCards #-}

module Recipe.Type(RecipeOptions(..), RecipeDetails(..), recipeDetails, ls) where

import General.Code


data RecipeOptions = RecipeOptions
    {recipeDir :: FilePath -- ^ Directory to use
    ,recipeThreads :: Int -- ^ Number of threads to use
    ,recipeRedownload :: Bool -- ^ Download everything from the web
    ,recipeRebuild :: Bool -- ^ Rebuild all local files
    }


data RecipeDetails = RecipeDetails
    {recipeOptions :: RecipeOptions
    ,download :: FilePath -> URL -> IO ()
    ,process :: [FilePath] -> [FilePath] -> IO () -> IO ()
    ,parallel_ :: [IO ()] -> IO ()
    }

    
recipeDetails :: RecipeOptions -> RecipeDetails
recipeDetails recipeOptions@RecipeOptions{..} = RecipeDetails{..}
    where
        parallel_ = sequence_

        download to url = do
            exists <- doesFileExist to
            when (not exists || recipeRedownload) $ do
                res <- system $ "wget " ++ url ++ " -O " ++ to
                when (res /= ExitSuccess) $ error $ "Failed to download " ++ url

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
    return $ filter f xs
