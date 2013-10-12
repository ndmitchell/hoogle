
module Recipe.General(convertSrc, convert, combine) where

import Recipe.Type
import Hoogle
import General.Base
import General.System
import System.Console.CmdArgs.Verbosity

txt x = map toLower x <.> "txt"
hoo x = map toLower x <.> "hoo"


convertSrc :: ([Name] -> IO ()) -> [Name] -> Name -> String -> IO ()
convertSrc make deps x src = do
    writeFileUtf8 (txt x) src
    make deps
    convert (map hoo deps) x (hoo x) src


---- convert a single database
convert :: [FilePath] -> Name -> FilePath -> String -> IO ()
convert deps x out src = do
    deps2 <- filterM doesFileExist deps
    when (deps /= deps2) $ putWarning $ "Warning: " ++ x ++ " doesn't know about dependencies on " ++ unwords (deps \\ deps2)
    dbs <- mapM loadDatabase deps2
    let (err,db) = createDatabase Haskell dbs src
    unless (null err) $ outStrLn $ "Skipped " ++ show (length err) ++ " warnings in " ++ x
    whenLoud $ outStr $ unlines $ map show err
    outStr $ "Converting " ++ x ++ "... "
    performGC
    saveDatabase out db
    outStrLn "done"


-- combine multiple databases
combine :: ([Name] -> IO ()) -> Name -> [Name] -> Bool -> IO ()
combine make x deps force = do
    make deps
    dbs <- mapM (loadDatabase . hoo) deps
    outStr $ "Creating " ++ x ++ " from " ++ show (length deps) ++ " databases... "
    performGC
    saveDatabase (hoo x) $ mconcat dbs
    outStrLn "done"
