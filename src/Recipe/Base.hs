{-# LANGUAGE RecordWildCards #-}
module Recipe.Base(base) where

import Recipe.Type
import Hoogle
import General.Code


base :: RecipeDetails -> [String] -> IO ()
base _ _ = writeFile "base.txt" ""
