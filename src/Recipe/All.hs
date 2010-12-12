-- Recipe actions:
-- Download to foo.src in most cases, then extract to foo.txt, which can later be compiled to foo.hoo
module Recipe.All(recipes) where

import General.Base
import General.System
import Control.Concurrent
import Recipe.Type

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
        ref <- newMVar []
        make ref opt ys $ map fst ys
        recapErrors
        putStrLn "Data generation complete"


make :: MVar [Name] -> CmdLine -> [(Name,[Name])] -> [Name] -> IO ()
make ref opt acts xs = forM_ xs $ \x -> do
    b <- modifyMVar ref $ \seen -> let b = x `elem` seen in return ([x|not b]++seen, b)
    unless b $ do
        putStrLn $ "Starting " ++ x
        case lookup x acts of
            Just ys | not $ null ys -> combine makeRec x ys True
            _ -> case x of
                "keyword" -> makeKeyword
                "default" -> combine makeRec x ["keyword","package","platform"] False
                "platform" -> makePlatform makeRec
                "package" -> makePackage
                "all" -> makeAll makeRec
                _ -> makeDefault makeRec (local opt) x
        putStrLn $ "Finished " ++ x
    where
        makeRec = make ref opt acts


parseActions :: [String] -> [(Name,[Name])]
parseActions [] = [("default",[])]
parseActions xs = map parseAction xs


parseAction :: String -> (Name,[Name])
parseAction x = (a, uncommas $ drop 1 b)
    where (a,b) = break (== '=') x
          uncommas = words . map (\x -> if x == ',' then ' ' else x)
