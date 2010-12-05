-- Recipe actions:
-- Download to foo.src in most cases, then extract to foo.txt, which can later be compiled to foo.hoo
module Recipe.All(recipes) where

import General.Code
import Recipe.Type
import Data.IORef

import Recipe.Download
import Recipe.Keyword
import Recipe.General
import Recipe.Hackage


-- CmdLine is guaranteed to be a constructor of type Data
recipes :: CmdLine -> IO ()
recipes opt = do
    hSetBuffering stdout NoBuffering
    createDirectoryIfMissing True $ datadir opt
    withDirectory (datadir opt) $ do
        resetErrors
        download opt
        let ys = parseActions $ actions opt
        ref <- newIORef []
        mapM_ (make ref ys) $ map fst ys
        recapErrors


make :: IORef [Name] -> [(Name,[Name])] -> Name -> IO ()
make ref acts x = do
    b <- fmap (x `elem`) $ readIORef ref
    unless b $ do
        modifyIORef ref (x:)
        putStrLn $ "Starting " ++ x
        case lookup x acts of
            Just ys | not $ null ys -> combine makeRec x ys True
            _ -> case x of
                "keyword" -> makeKeyword
                "default" -> combine makeRec x ["keyword","package","platform"] False
                "platform" -> makePlatform makeRec
                "package" -> makePackage
                "all" -> makeAll makeRec
                _ -> makeDefault makeRec x
        putStrLn $ "Finished " ++ x
    where
        makeRec = make ref acts


parseActions :: [String] -> [(Name,[Name])]
parseActions [] = [("default",[])]
parseActions xs = map parseAction xs


parseAction :: String -> (Name,[Name])
parseAction x = (a, uncommas $ drop 1 b)
    where (a,b) = break (== '=') x
          uncommas = words . map (\x -> if x == ',' then ' ' else x)
