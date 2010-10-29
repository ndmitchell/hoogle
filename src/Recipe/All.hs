-- Recipe actions:
-- Download to foo.src in most cases, then extract to foo.txt, which can later be compiled to foo.hoo
module Recipe.All(recipes, RecipeOptions(..)) where

import General.Code
import Recipe.Type

import Recipe.Keyword
import Recipe.General
import Recipe.Hackage


recipes :: RecipeOptions -> [String] -> IO ()
recipes opt xs | not $ null es = error $ unlines es
               | otherwise = sequence_ ys
    where (es,ys) = unzipEithers $ map (recipe opt) $ if null xs then words defaultRecipe else xs


recipe :: RecipeOptions -> String -> Either String (IO ())
recipe opt x = case lookup a list of
        Nothing -> Left $ "Unknown recipe: " ++ a
        Just (_,act) -> Right $ do
            putStrLn $ "Running recipe: " ++ x
            withDirectory (recipeDir opt) $ act (recipeDetails opt) (drop 1 b)
            putStrLn $ "Finished recipe"
    where (a,b) = break (== '=') x


defaultRecipe = "keyword package convert hackage platform default=keyword,platform all=keyword,hackage"

list = let f a b c = (a,(b,c)) in
    [f "help" "Display this help message" help
    ,f "keyword" "Create textbase for keywords, from the Haskell Wiki" keyword
    ,f "package" "Create textbases for all packages on Hackage" package
    ,f "convert" "Create databases for all textbases" convert
    ,f "hackage" "Create hackage.hoo for all of Hackage" hackage
    ,f "platform" "Create platform.hoo for the Haskell Platform" platform
    ,f "default" "Create default.hoo from all databases (or a given subset)" (multiple "default")
    ,f "all" "Create all.hoo from all databases (or a given subset)" (multiple "all")
    ]


help :: RecipeDetails -> String -> IO ()
help opts args = putStr $ unlines $
        "Download and generate data files" :
        "" :
        ["  " ++ a ++ replicate (n + 2 - length a) ' ' ++ b | (a,(b,_)) <- list] ++
        [""
        ,"The default recipe is:"
        ,"  " ++ defaultRecipe]
    where
        n = maximum $ map (length . fst) list
