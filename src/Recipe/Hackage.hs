
module Recipe.Hackage(makePlatform, makeDefault, makePackage, makeAll) where

import Recipe.Type
import Recipe.Cabal
import Recipe.General
import General.Base
import General.System
import General.Util
import General.Web
import Control.Exception


-- FIXME: This is a list of hack
avoid = words "ghc-prim integer integer-simple integer-gmp rts ghc Win32"


makePlatform :: ([Name] -> IO ()) -> IO ()
makePlatform make = do
    xs <- listPlatform
    forM_ xs $ \(name,ver) -> do
        v2 <- version cabals name
        when (ver /= v2) $ putStrLn $ "Warning: Version mismatch for " ++ name ++ " (platform=" ++ ver ++ ", cabal=" ++ v2 ++ ")"
    combine make "platform" (map fst xs) False


makeAll :: ([Name] -> IO ()) -> IO ()
makeAll make = do
    xs <- listing inputs
    make xs


-- create a database containing an entry for each package in hackage
makePackage :: IO ()
makePackage = do
    xs <- listing cabals
    xs <- forM xs $ \name -> do
        ver <- version cabals name
        let file = cabals </> name </> ver </> name <.> "cabal"
        src <- readCabal file
        return $ [""] ++ zipWith (++) ("-- | " : repeat "--   ") (cabalDescription src) ++
                 ["--","-- Version " ++ ver, "@url package/" ++ name, "@entry package " ++ name]
    convertSrc noDeps "package" $ unlines $
        "@url http://hackage.haskell.org/" : "@package package" : concat xs


makeDefault :: ([Name] -> IO ()) -> [FilePath] -> Name -> IO ()
makeDefault make local "ghc" = do
    had <- try $ readFileUtf8' "download/ghc.txt"
    case had of
        Left e -> putWarning $ "Warning: Exception when reading haddock for ghc, " ++ show (e :: SomeException)
        Right had -> do
            convertSrc make "ghc" $ unlines $ "@depends base" : concatMap f (haddockHacks $ lines had)
    where
        f x | "@package " `isPrefixOf` x = ["@url http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/",x]
            | otherwise = [x]

makeDefault make local name = do
    let base = name == "base"
    b1 <- doesDirectoryExist $ cabals </> name
    b2 <- doesDirectoryExist $ inputs </> name
    if not base && (not b1 || not b2) then
        putWarning $ "Warning: " ++ name ++ " couldn't find both Cabal and Haddock inputs"
     else do
        vc <- version cabals name
        vh <- if base then return vc else version inputs name
        when (vc /= vh) $ putStrLn $ "Warning: Version mismatch for " ++ name ++ " (cabal=" ++ vc ++ ", haddock=" ++ vh ++ ")"
        let had = if base then "download/base.txt" else inputs </> name </> vh </> "doc" </> "html" </> name <.> "txt"
            cab = cabals </> name </> vc </> name <.> "cabal"
        had <- try $ readFileUtf8' had
        case had of
            Left e -> putWarning $ "Warning: Exception when reading haddock for " ++ name ++ ", " ++ show (e :: SomeException)
            Right had -> do
                cab <- readCabal cab
                loc <- findLocal local name
                convertSrc make name $ unlines $
                    ["@depends " ++ a | a <- cabalDepends cab \\ (name:avoid)] ++
                    (maybe id haddockPackageUrl loc) (haddockHacks $ lines had)


-- try and find a local filepath
findLocal :: [FilePath] -> Name -> IO (Maybe URL)
findLocal paths name = fmap (listToMaybe . concat . concat) $ forM paths $ \p -> do
    xs <- getDirectoryContents p
    xs <- return [p </> x | x <- reverse $ sort xs, name == fst (rbreak (== '-') x)] -- make sure highest version comes first
    forM xs $ \x -> do
        b <- doesDirectoryExist $ x </> "html"
        x <- return $ if b then x </> "html" else x
        b <- doesFileExist $ x </> "doc-index.html"
        return [filePathToURL $ x </> "index.html" | b]


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
           , not $ avoid name]
    where
        avoid x = ("haskell" `isPrefixOf` x && all isDigit (drop 7 x)) ||
                  (x `elem` words "Cabal hpc Win32")


---------------------------------------------------------------------
-- HADDOCK HACKS

-- Eliminate @version
-- Change :*: to (:*:), Haddock bug
-- Change !!Int to !Int, Haddock bug
-- Change instance [overlap ok] to instance, Haddock bug
-- Change instance [incoherent] to instance, Haddock bug
-- Change !Int to Int, HSE bug

haddockHacks :: [String] -> [String]
haddockHacks = map (unwords . map f . words) . filter (not . isPrefixOf "@version ")
    where
        f "::" = "::"
        f (':':xs) = "(:" ++ xs ++ ")"
        f ('!':'!':x:xs) | isAlpha x = xs
        f ('!':x:xs) | isAlpha x || x `elem` "[(" = x:xs
        f x | x `elem` ["[overlap","ok]","[incoherent]"] = ""
        f x = x


haddockPackageUrl :: URL -> [String] -> [String]
haddockPackageUrl x = concatMap f
    where f y | "@package " `isPrefixOf` y = ["@url " ++ x, y]
              | otherwise = [y]
