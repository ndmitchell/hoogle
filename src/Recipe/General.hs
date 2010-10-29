{-# LANGUAGE RecordWildCards #-}
module Recipe.General(convert, combine) where

import Recipe.Type
import Hoogle
import General.Code
import Data.Monoid


convert :: RecipeDetails -> String -> IO ()
convert RecipeDetails{..} _ = do
    xs <- ls $ \x -> takeExtension x == ".txt"
    parallel_ $ flip map xs $ \from -> let to = replaceExtension from "hoo" in process [from] [to] $ do
        putStrLn $ "Converting " ++ from
        src <- readFile from
        let db = case createDatabase [] src of
                Left x -> error $ "Parse error with " ++ from ++ "\n" ++ show x
                Right x -> x
        saveDatabase to db
        putStrLn $ "Written " ++ to


combine :: RecipeDetails -> String -> IO ()
combine RecipeDetails{..} _ = do
    let to = "default.hoo"
    from <- ls $ \x -> takeExtension x == ".hoo" && x /= to
    process from [to] $ do
        putStrLn $ "Combining " ++ show (length from) ++ " databases"
        xs <- mapM loadDatabase from
        saveDatabase to $ mconcat xs
