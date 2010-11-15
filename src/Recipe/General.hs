{-# LANGUAGE RecordWildCards #-}
module Recipe.General(convert, multiple) where

import Recipe.Type
import Hoogle
import General.Code
import System.Console.CmdArgs.Verbosity


convert :: RecipeDetails -> [String] -> IO ()
convert RecipeDetails{..} xs = do
    xs <- if null xs
        then ls $ \x -> takeExtension x == ".txt"
        else return $ map (<.> "txt") xs
    xs <- order xs
    forM_ xs $ \from -> let to = replaceExtension from "hoo" in process [from] [to] $ do
        putStrLn $ "Converting " ++ from
        (deps,src) <- readInput from
        let deps2 = map (<.> "hoo") deps
        deps3 <- filterM doesFileExist deps2
        when (deps2 /= deps3) $ putStrLn $ "Some dependencies not already built: " ++ unwords (deps2 \\ deps3)
        dbs <- mapM loadDatabase deps3
        let (err,db) = createDatabase Haskell dbs src
        unless (null err) $ putStrLn $ "Skipped " ++ show (length err) ++ " errors in " ++ from
        whenLoud $ putStr $ unlines $ map show err
        saveDatabase to db
        putStrLn $ "Written " ++ to


readInput :: String -> IO ([String], String)
readInput x = do
    src <- readFile x
    let (a,b) = span ("@depends " `isPrefixOf`) $ lines src
    return (map (drop 9) a, unlines b)

order :: [String] -> IO [String]
order xs = do
    ys <- forM xs $ \x -> do
        (a,b) <- readInput $ x
        return (dropExtension x, a)
    fmap (map (<.> "txt")) $ f ys
    where
        f [] = return []
        f xs = do
            let (now,next) = partition (all (`notElem` map fst xs) . snd) xs
            if null now then do
                putStrLn $ "Cycle detected with " ++ unwords (map fst xs)
                return $ map fst xs
             else do
                rest <- f next
                return $ map fst now ++ rest


-- database that were combined from multiple databases
composite = words "hackage platform default all"

multiple :: String -> RecipeDetails -> [String] -> IO ()
multiple to RecipeDetails{..} xs = do
    from <- if null xs
        then ls $ \x -> takeExtension x == ".hoo" && dropExtension x `notElem` composite
        else return $ map (<.> "hoo") xs
    combine (to <.> "hoo") from 
