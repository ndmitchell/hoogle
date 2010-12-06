
module Recipe.Hackage(makePlatform, makeDefault, makePackage, makeAll) where

import Recipe.Type
import Recipe.General
import General.Base
import System.Directory
import System.IO


avoid = words "ghc-prim integer integer-simple integer-gmp rts ghc Win32"


makePlatform :: (Name -> IO ()) -> IO ()
makePlatform make = do
    xs <- listPlatform
    forM_ xs $ \(name,ver) -> do
        v2 <- version cabals name
        when (ver /= v2) $ putStrLn $ "Warning: Version mismatch for " ++ name ++ " (platform=" ++ ver ++ ", cabal=" ++ v2 ++ ")"
    combine make "platform" (map fst xs) False


makeAll :: (Name -> IO ()) -> IO ()
makeAll make = do
    xs <- listing haddocks
    mapM_ make xs


-- create a database containing an entry for each package in hackage
makePackage :: IO ()
makePackage = do
    xs <- listing cabals
    xs <- forM xs $ \name -> do
        ver <- version cabals name
        let file = cabals </> name </> ver </> name <.> "cabal"
        src <- readCabal file
        return $ [""] ++ zipWith (++) ("-- | " : repeat "--   ") (cabalDescription src) ++
                 ["--","-- Version " ++ ver, "@package " ++ name]
    writeFile "package.txt" $ unlines $ concat xs
    convert noDeps "package"


makeDefault :: (Name -> IO ()) -> Name -> IO ()
makeDefault make name = do
    b1 <- doesDirectoryExist $ cabals </> name
    b2 <- doesDirectoryExist $ haddocks </> name
    if not b1 || not b2 then
        putError $ "Error: " ++ name ++ " couldn't find both Cabal and Haddock inputs"
     else do
        vc <- version cabals name
        vh <- version haddocks name
        when (vc /= vh) $ putStrLn $ "Warning: Version mismatch for " ++ name ++ " (cabal=" ++ vc ++ ", haddock=" ++ vh ++ ")"
        let had = haddocks </> name </> vh </> name <.> "txt"
            cab = cabals </> name </> vc </> name <.> "cabal"
        h <- openFile had ReadMode
        sz <- hFileSize h
        hClose h
        if sz == 0 then
            putError $ "Error: " ++ name ++ " has no haddock output"
         else do
            had <- readFile' had
            cab <- readCabal cab
            writeFile (name <.> "txt") $
                unlines ["@depends " ++ a | a <- cabalDepends cab, a `notElem` avoid] ++ "\n" ++
                haddockHacks had
            convert make name


---------------------------------------------------------------------
-- READ PLATFORM

listPlatform :: IO [(Name,String)]
listPlatform = do
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
