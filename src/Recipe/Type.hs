-- Recipe actions:
-- Download to foo.src in most cases, then extract to foo.txt, which can later be compiled to foo.hoo
module Recipe.Type(RecipeOptions(..)) where

data RecipeOptions = RecipeOptions
    {recipeDir :: FilePath -- ^ Directory to use
    ,recipeThreads :: Int -- ^ Number of threads to use
    ,recipeRedownload :: Bool -- ^ Download everything from the web
    ,recipeRebuild :: Bool -- ^ Rebuild all local files
    }
