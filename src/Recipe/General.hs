
module Recipe.General(convert, combine) where

import Recipe.Type
import Hoogle
import General.Code
import System.Console.CmdArgs.Verbosity
import System.Mem


-- convert a single database
convert :: (Name -> IO ()) -> Name -> IO ()
convert make x = do
    b <- doesFileExist $ x <.> "txt"
    if not b then
        putError $ "Error: " ++ x ++ " couldn't be converted, no input file found"
     else do
        (deps,src) <- readInput x
        mapM_ make deps
        let deps2 = map hoo deps
        deps3 <- filterM doesFileExist deps2
        when (deps2 /= deps3) $ putError $ "Error: " ++ x ++ " doesn't know about dependencies on " ++ unwords (deps2 \\ deps3)
        dbs <- mapM loadDatabase deps3
        let (err,db) = createDatabase Haskell dbs src
        unless (null err) $ putStrLn $ "Skipped " ++ show (length err) ++ " errors in " ++ x
        whenLoud $ putStr $ unlines $ map show err
        putStr $ "Converting " ++ x ++ "... "
        performGC
        saveDatabase (hoo x) db
        putStrLn "done"


readInput :: Name -> IO ([Name], String)
readInput x = do
    src <- readFile $ x <.> "txt"
    let (a,b) = span ("@depends " `isPrefixOf`) $ lines src
    return (map (drop 9) a, unlines b)



-- combine multiple databases
combine :: (Name -> IO ()) -> Name -> [Name] -> Bool -> IO ()
combine make x deps force = do
    mapM_ make deps
    dbs <- mapM (loadDatabase . hoo) deps
    putStr $ "Creating " ++ x ++ " from " ++ show (length deps) ++ " databases... "
    performGC
    saveDatabase (hoo x) $ mconcat dbs
    putStrLn "done"
