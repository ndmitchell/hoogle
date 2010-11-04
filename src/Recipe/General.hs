{-# LANGUAGE RecordWildCards #-}
module Recipe.General(convert, multiple) where

import Recipe.Type
import Hoogle
import General.Code


convert :: RecipeDetails -> [String] -> IO ()
convert RecipeDetails{..} xs = do
    xs <- if null xs
        then ls $ \x -> takeExtension x == ".txt"
        else return $ map (<.> "txt") xs
    par $ flip map xs $ \from -> let to = replaceExtension from "hoo" in process [from] [to] $ do
        putStrLn $ "Converting " ++ from
        src <- readFile from
        let (err,db) = createDatabase [] src
        unless (null err) $ putStrLn $ "Skipped " ++ show (length err) ++ " errors in " ++ from
        saveDatabase to db
        putStrLn $ "Written " ++ to


-- database that were combined from multiple databases
composite = words "hackage platform default all"

multiple :: String -> RecipeDetails -> [String] -> IO ()
multiple to RecipeDetails{..} xs = do
    from <- if null xs
        then ls $ \x -> takeExtension x == ".hoo" && dropExtension x `notElem` composite
        else return $ map (<.> "hoo") xs
    combine (to <.> "hoo") from 
