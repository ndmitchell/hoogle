{-# LANGUAGE RecordWildCards #-}
module Recipe.General(convert, multiple) where

import Recipe.Type
import Hoogle
import General.Code


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


-- database that were combined from multiple databases
composite = words "hackage platform default all"

multiple :: String -> RecipeDetails -> String -> IO ()
multiple to RecipeDetails{..} xs = do
    from <- if null xs
        then ls $ \x -> takeExtension x == ".hoo" && dropExtension x `notElem` composite
        else return $ map (<.> "hoo") $ words $ map (\x -> if x == ',' then ' ' else x) xs
    combine (to <.> "hoo") from 
