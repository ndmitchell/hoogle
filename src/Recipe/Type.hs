{-# LANGUAGE RecordWildCards #-}

module Recipe.Type(
    CmdLine(..), Name, hoo, noDeps,
    resetBuilt, build,
    resetErrors, putError, recapErrors,
    downloadMay, download, buildFrom, system_,
    Cabal(..), readCabal, readCabalDepends, readCabalField
    ) where

import CmdLine.All
import Data.IORef
import System.IO.Unsafe
import General.Code


type Name = String

hoo :: Name -> FilePath
hoo x = x <.> "hoo"


noDeps :: Name -> IO ()
noDeps = error "Internal error: package with no dependencies had dependencies"


---------------------------------------------------------------------
-- BUILT CACHE

{-# NOINLINE built #-}
built :: IORef [FilePath]
built = unsafePerformIO $ newIORef []


resetBuilt :: IO ()
resetBuilt = writeIORef built []


addBuilt :: FilePath -> IO ()
addBuilt x = modifyIORef built (x:)


isBuilt :: FilePath -> IO Bool
isBuilt x = fmap (x `elem`) $ readIORef built


build :: FilePath -> IO () -> IO ()
build x act = do
    b <- isBuilt x
    unless b $ do addBuilt x; act

---------------------------------------------------------------------
-- ERROR MESSAGES

{-# NOINLINE errors #-}
errors :: IORef [String]
errors = unsafePerformIO $ newIORef []

putError :: String -> IO ()
putError x = do
    putStrLn x
    modifyIORef errors (x:)

recapErrors :: IO ()
recapErrors = do
    xs <- readIORef errors
    mapM_ putStrLn $ reverse xs

resetErrors :: IO ()
resetErrors = writeIORef errors []


---------------------------------------------------------------------
-- OPERATIONS

downloadMay :: CmdLine -> FilePath -> URL -> IO Bool
downloadMay opt fil url = do
    build fil $ do
        b <- doesFileExist fil
        when (not b || redownload opt) $ do
            res <- system $ "wget " ++ url ++ " -O " ++ fil
            let b = res == ExitSuccess
            unless b $ removeFile fil
    doesFileExist fil


download :: CmdLine -> FilePath -> URL -> IO ()
download opt fil url = do
    b <- downloadMay opt fil url
    unless b $ error $ "Failed to download " ++ url


-- warning: if the action takes less than a second to complete
-- next time round it may still invoke buildFrom
buildFrom :: CmdLine -> FilePath -> [FilePath] -> IO () -> IO ()
buildFrom opt out deps act = do
    let act2 = do putStrLn $ "# " ++ out; act
    b <- doesFileExist out
    if not b || rebuild opt then act2 else do
        old <- fmap maximum $ mapM getModificationTime deps
        new <- getModificationTime out
        when (old >= new) act2


system_ :: String -> IO ()
system_ x = do
    res <- system x
    when (res /= ExitSuccess) $ error $ "System command failed: " ++ x


---------------------------------------------------------------------
-- CABAL

data Cabal = Cabal {cabalDepends :: [String], cabalDescription :: [String]}

readCabal :: FilePath -> IO Cabal
readCabal file = do
    src <- fmap lines $ readFile' file
    return $ Cabal (readCabalDepends src) (readCabalField src True "description")


readCabalDepends :: [String] -> [String]
readCabalDepends xs = nub $ map (takeWhile g) $ filter f $ words $ map (rep ',' ' ') $ unwords $ readCabalField xs False "build-depends"
    where f x = x /= "" && isAlpha (head x)
          g x = isAlphaNum x || x `elem` "-_"


readCabalField :: [String] -> Bool -> String -> [String]
readCabalField xs root name = f xs
    where
        f (x:xs) | (name ++ ":") `isPrefixOf` map toLower x2 && (null spc || not root) =
                [x4 | x4 /= []] ++ map (rep "." "" . trim) ys ++ f zs
            where
                x4 = trim x3
                x3 = drop (length name + 1) x2
                (spc,x2) = span isSpace x
                (ys,zs) = span ((> length spc) . length . takeWhile isSpace) xs
        f (x:xs) = f xs
        f [] = []
