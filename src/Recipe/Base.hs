{-# LANGUAGE RecordWildCards #-}
module Recipe.Base(base) where

import Recipe.Type


base :: RecipeDetails -> [String] -> IO ()
base _ _ = writeFile "base.txt" ""
