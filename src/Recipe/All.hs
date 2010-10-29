-- Recipe actions:
-- Download to foo.src in most cases, then extract to foo.txt, which can later be compiled to foo.hoo
module Recipe.All(recipes, RecipeOptions(..)) where

import General.Code
import Recipe.Type

import Recipe.Keyword
import Recipe.Combine
import Recipe.Convert


recipes :: RecipeOptions -> [String] -> IO ()
recipes opt xs | not $ null es = error $ unlines es
               | otherwise = sequence_ ys
    where (es,ys) = unzipEithers $ map (recipe opt) xs


recipe :: RecipeOptions -> String -> Either String (IO ())
recipe opt x = case lookup a list of
        Nothing -> Left $ "Unknown recipe: " ++ a
        Just (_,act) -> Right $ do
            putStrLn $ "Running recipe: " ++ x
            withDirectory (recipeDir opt) $ act (recipeDetails opt) (drop 1 b)
            putStrLn $ "Finished recipe"
    where (a,b) = break (== '=') x



list = let f a b c = (a,(b,c)) in
    [f "help" "Display this help message" help
    ,f "keyword" "Create a keyword list from the Haskell Wiki" keyword
    ,f "convert" "Convert all databases to .hoo format" convert
    ,f "combine" "Combine all databases to produce default.hoo" combine
    ]


help :: RecipeDetails -> String -> IO ()
help opts args = putStr $ unlines $
        "Download and generate data files" :
        "" :
        ["  " ++ a ++ replicate (n + 2 - length a) ' ' ++ b | (a,(b,_)) <- list]
    where
        n = maximum $ map (length . fst) list
