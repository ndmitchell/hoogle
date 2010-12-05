{-# LANGUAGE RecordWildCards #-}
module Recipe.Hackage(makePlatform, makeDefault, makePackage, makeAll) where

import Recipe.Type
import Recipe.General
import General.Code
import Data.Function


avoid = words "ghc-prim integer integer-simple integer-gmp rts ghc Win32"


platform = "download/haskell-platform.cabal"
cabals = "download/hackage-cabal"
haddocks = "download/hackage-haddock"
tgz x = x <.> "tar.gz"


makePlatform :: CmdLine -> (Name -> IO ()) -> IO ()
makePlatform opt make = do
    xs <- listPlatform opt
    buildFrom opt "platform.hoo" [platform] $ do
        b <- doesFileExist "platform.hoo"
        when b $ removeFile "platform.hoo"
    forM xs $ \(name,ver) -> do
        v2 <- verCabals opt name
        when (ver /= v2) $ putStrLn $ "Warning: Version mismatch for " ++ name ++ " (platform=" ++ ver ++ ", cabal=" ++ v2 ++ ")"
    combine opt make "platform" (map fst xs) False


makeAll :: CmdLine -> (Name -> IO ()) -> IO ()
makeAll opt make = do
    xs <- listHaddocks opt
    mapM_ make xs


-- create a database containing an entry for each package in hackage
makePackage :: CmdLine -> IO ()
makePackage opt = do
    ensureCabals opt
    buildFrom opt "package.txt" [tgz cabals] $ do
        xs <- listCabals opt
        xs <- forM xs $ \name -> do
            ver <- verCabals opt name
            let file = cabals </> name </> ver </> name <.> "cabal"
            src <- readCabal file
            return $ [""] ++ zipWith (++) ("-- | " : repeat "--   ") (cabalDescription src) ++
                     ["--","-- Version " ++ ver, "@package " ++ name]
        writeFile "package.txt" $ unlines $ concat xs
    convert opt noDeps "package"


makeDefault :: CmdLine -> (Name -> IO ()) -> Name -> IO ()
makeDefault opt make name = do
    b1 <- doesDirectoryExist $ cabals </> name
    b2 <- doesDirectoryExist $ haddocks </> name
    if not b1 || not b2 then
        putError $ "Couldn't find database for " ++ name
     else do
        vc <- verCabals opt name
        vh <- verHaddocks opt name
        when (vc /= vh) $ putStrLn $ "Warning: Version mismatch for " ++ name ++ " (cabal=" ++ vc ++ ", haddock=" ++ vh ++ ")"
        let had = haddocks </> name </> vh </> name <.> "txt"
            cab = cabals </> name </> vc </> name <.> "cabal"
        buildFrom opt (name <.> "txt") [cab,had] $ do
            h <- openFile had ReadMode
            sz <- hFileSize h
            hClose h
            if sz == 0 then
                putError $ "Error: No haddock input for " ++ name
             else do
                had <- readFile' had
                cab <- readCabal cab
                writeFile (name <.> "txt") $
                    unlines ["@depends " ++ a | a <- cabalDepends cab, a `notElem` avoid] ++ "\n" ++
                    haddockHacks had
        convert opt make name


---------------------------------------------------------------------
-- UTILITIES

ensureCabals opt = ensure opt cabals "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
listCabals opt = do ensureCabals opt; list cabals
verCabals opt x = do ensureCabals opt; ver cabals x

ensureHaddocks opt | haddock opt = downloadHaddocks opt
                   | otherwise = ensure opt haddocks "http://haskell.org/hoogle/hackage-haddock.tar.gz"
listHaddocks opt = do ensureHaddocks opt; list haddocks
verHaddocks opt x = do ensureHaddocks opt; ver haddocks x


ensure :: CmdLine -> FilePath -> URL -> IO ()
ensure opt out url = build (out <.> "txt") $ do
    download opt (tgz out) url
    buildFrom opt{rebuild=False} (out <.> "txt") [tgz out] $ do
        createDirectoryIfMissing True out
        withDirectory out $ do
            system_ $ "tar -xf .." </> tgz (takeFileName out)
        writeFile (out <.> "txt") ""

list :: FilePath -> IO [Name]
list dir = do
    xs <- getDirectoryContents dir
    return $ sortBy (compare `on` map toLower) $ filter (`notElem` [".","..","preferred-versions"]) xs

ver :: FilePath -> Name -> IO String
ver dir x = do
    ys <- getDirectoryContents $ dir </> x
    when (null ys) $ error $ "Couldn't find version for " ++ x ++ " in " ++ dir
    let f = map (read :: String -> Int) . words . map (\x -> if x == '.' then ' ' else x)
    return $ maximumBy (compare `on` f) $ filter (all (not . isAlpha)) ys


downloadHaddocks :: CmdLine -> IO ()
downloadHaddocks opt = build (haddocks <.> "txt") $ do
    xs <- listCabals opt
    forM_ xs $ \name -> do
        ver <- verCabals opt name
        let out = haddocks </> name </> ver </> name <.> "txt"
            url = "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ ver ++ "/doc/html/" ++ name ++ ".txt"
        createDirectoryIfMissing True $ takeDirectory out
        b <- downloadMay opt out url
        unless b $ writeFile out ""
    withDirectory haddocks $ system_ "tar -czf../hackage-haddock.tar.gz *"
    writeFile (haddocks <.> "txt") ""


listPlatform :: CmdLine -> IO [(Name,String)]
listPlatform opt = do
    download opt platform "http://code.haskell.org/haskell-platform/haskell-platform.cabal"
    src <- readFile platform
    let xs = takeWhile (not . isPrefixOf "build-tools:" . ltrim) $
             dropWhile (not . isPrefixOf "build-depends:" . ltrim) $
             lines src
    return [(name, takeWhile (\x -> x == '.' || isDigit x) $ drop 1 b)
           | x <- xs, (a,_:b) <- [break (== '=') x], let name = trim $ dropWhile (== '-') $ trim a
           , name `notElem` words "Cabal hpc Win32"]


---------------------------------------------------------------------
-- HADDOCK HACKS

-- Eliminate @version
-- Change :*: to (:*:), Haddock bug
-- Change !!Int to !Int, Haddock bug
-- Change instance [overlap ok] to instance, Haddock bug
-- Change instance [incoherent] to instance, Haddock bug
-- Change !Int to Int, HSE bug

haddockHacks :: String -> String
haddockHacks = unlines . map (unwords . map f . words) . filter (not . isPrefixOf "@version ") . lines
    where
        f "::" = "::"
        f (':':xs) = "(:" ++ xs ++ ")"
        f ('!':'!':x:xs) | isAlpha x = xs
        f ('!':x:xs) | isAlpha x || x `elem` "[(" = x:xs
        f x | x `elem` ["[overlap","ok]","[incoherent]"] = ""
        f x = x
