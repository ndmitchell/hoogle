{-# LANGUAGE ScopedTypeVariables #-}
module Console.Rank(rank) where

import General.Base
import Hoogle


rank :: FilePath -> IO ()
rank file = do
    src <- readFile' file
    res <- scoring $ scores $ parse $ lines src
    putStrLn res


scores :: ([String], [(String,[String])]) -> [(Score,Score)]
scores (pre,xs) = concatMap trans
    [
        [ fst $ head $ search db q ++ [error $ "Did not find in " ++ query ++ ", " ++ y]
        | y <- ys
        , let (err::String,db) = (error "this feature has been disabled" createDatabase) Haskell [] $ unlines $ pre ++ ["a::" ++ y]
        , null err || error "Errors while converting rank database"
        ]
    | (query,ys) <- xs, let q = right ("Could not parse query: " ++ query) $ parseQuery Haskell query]
    where right msg = either (\e -> error $ msg ++ "\n" ++ show e) id

trans (x:xs) = map ((,) x) xs ++ trans xs
trans [] = []


parse :: [String] -> ([String], [(String,[String])])
parse src = (db, [(drop 6 x, filter isReal $ takeWhile (not . isRank) xs) | x:xs <- tails rest, isRank x])
    where
        isReal x = not $ all isSpace x || "--" `isPrefixOf` x
        isRank = isPrefixOf "@rank "
        (db,rest) = break isRank src
