-- Recipe actions:
-- Download to foo.src in most cases, then extract to foo.txt, which can later be compiled to foo.hoo
module Recipe.All(recipes) where

import General.Code
import Recipe.Type

import Recipe.Keyword
import Recipe.General
import Recipe.Hackage


-- CmdLine is guaranteed to be a constructor of type Data
recipes :: CmdLine -> IO ()
recipes opt = withDirectory (datadir opt) $ do
    resetBuilt
    resetErrors
    let ys = parseActions $ actions opt
    mapM_ (make opt ys) $ map fst ys
    recapErrors


parseActions :: [String] -> [(Name,[Name])]
parseActions [] = [("default",[])]
parseActions xs = map parseAction xs


parseAction :: String -> (Name,[Name])
parseAction x = (a, uncommas $ drop 1 b)
    where (a,b) = break (== '=') x
          uncommas = words . map (\x -> if x == ',' then ' ' else x)


make :: CmdLine -> [(Name,[Name])] -> Name -> IO ()
make opt acts x = build (hoo x) $ do
    putStrLn $ "Starting " ++ x
    case lookup x acts of
        Just ys | not $ null ys -> combine opt makeRec x ys True
        _ -> case x of
            "keyword" -> makeKeyword opt
            "default" -> combine opt makeRec x ["keyword","package","platform"] False
            "platform" -> makePlatform opt makeRec
            "package" -> makePackage opt
            "all" -> makeAll opt makeRec
            _ -> makeDefault opt makeRec x
    putStrLn $ "Finished " ++ x
    where
        makeRec = make opt acts
