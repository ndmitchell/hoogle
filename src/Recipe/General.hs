
module Recipe.General(convertSrc, convert, combine) where

import Recipe.Type
import Hoogle
import General.Base
import General.System
import System.Console.CmdArgs.Verbosity

txt x = map toLower x <.> "txt"
hoo x = map toLower x <.> "hoo"


convertSrc :: ([Name] -> IO ()) -> Name -> String -> IO ()
convertSrc make x src = do
    writeFileUtf8 (txt x) src
    convert make x


-- convert a single database
convert :: ([Name] -> IO ()) -> Name -> IO ()
convert make x = do
    b <- doesFileExist $ txt x
    if not b then
        putWarning $ "Warning: " ++ x ++ " couldn't be converted, no input file found"
     else do
        (deps,src) <- readInput x
        make deps
        let deps2 = map hoo deps
        deps3 <- filterM doesFileExist deps2
        when (deps2 /= deps3) $ putWarning $ "Warning: " ++ x ++ " doesn't know about dependencies on " ++ unwords (deps2 \\ deps3)
        dbs <- mapM loadDatabase deps3
        let (err,db) = createDatabase Haskell dbs src
        unless (null err) $ outStrLn $ "Skipped " ++ show (length err) ++ " warnings in " ++ x
        whenLoud $ outStr $ unlines $ map show err
        outStr $ "Converting " ++ x ++ "... "
        performGC
        saveDatabase (hoo x) db
        outStrLn "done"


readInput :: Name -> IO ([Name], String)
readInput x = do
    src <- readFileUtf8 $ txt x
    let (a,b) = span ("@depends " `isPrefixOf`) $ lines src
    return (map (drop 9) a, unlines b)



-- combine multiple databases
combine :: ([Name] -> IO ()) -> Name -> [Name] -> Bool -> IO ()
combine make x deps force = do
    make deps
    dbs <- mapM (loadDatabase . hoo) deps
    outStr $ "Creating " ++ x ++ " from " ++ show (length deps) ++ " databases... "
    performGC
    saveDatabase (hoo x) $ mconcat dbs
    outStrLn "done"
