{-# LANGUAGE RecordWildCards #-}
module Recipe.General(convert, combine) where

import Recipe.Type
import Hoogle
import General.Code
import System.Console.CmdArgs.Verbosity
import System.Mem


-- convert a single database
convert :: CmdLine -> (Name -> IO ()) -> Name -> IO ()
convert opt make x = do
    b <- doesFileExist $ x <.> "txt"
    if not b then
        putError $ "Error: Couldn't convert " ++ x ++ ", no input file found"
     else do
        (deps,src) <- readInput x
        mapM_ make deps
        let deps2 = map hoo deps
        deps3 <- filterM doesFileExist deps2
        when (deps2 /= deps3) $ putError $ "Error: Some dependencies not built: " ++ unwords (deps2 \\ deps3)
        buildFrom opt (hoo x) ((x <.> "txt") : deps3) $ do
            dbs <- mapM loadDatabase deps3
            let (err,db) = createDatabase Haskell dbs src
            unless (null err) $ putStrLn $ "Skipped " ++ show (length err) ++ " errors in " ++ x
            whenLoud $ putStr $ unlines $ map show err
            performGC
            saveDatabase (hoo x) db


readInput :: Name -> IO ([Name], String)
readInput x = do
    src <- readFile $ x <.> "txt"
    let (a,b) = span ("@depends " `isPrefixOf`) $ lines src
    return (map (drop 9) a, unlines b)



-- combine multiple databases
combine :: CmdLine -> (Name -> IO ()) -> Name -> [Name] -> Bool -> IO ()
combine opt make x deps force = (if force then id else buildFrom opt (hoo x) (map hoo deps)) $ do
    mapM_ make deps
    dbs <- mapM (loadDatabase . hoo) deps
    putStrLn $ "Combining " ++ show (length deps) ++ " databases"
    performGC
    saveDatabase (hoo x) $ mconcat dbs
