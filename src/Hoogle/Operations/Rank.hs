
module Hoogle.Operations.Rank(rank) where

import General.Code
import Hoogle.TextBase.All
import Hoogle.DataBase.All
import Hoogle.TypeSig.All


rank :: FilePath -> IO ()
rank file = do
    tests <- liftM readRankTests $ readFile file
    error $ show tests



data RankTest = RankTest TypeSig [TypeSig]
                deriving Show


readRankTests :: String -> [RankTest]
readRankTests = join . concatMap parse . lines
    where
        parse :: String -> [(Bool, TypeSig)]
        parse xs | null xs || "--" `isPrefixOf` xs = []
        parse ('@':xs) = [(True,f xs)]
        parse xs = [(False,f xs)]

        f xs = case parseTypeSig xs of
                   Right x -> x
                   Left x -> error $ "readRank failed to parse " ++ show xs ++ ", " ++ show x

        join ((True,t):xs) = RankTest t (map snd a) : join b
            where (a,b) = break fst xs
        join [] = []
        join ((False,t):_) = error $ "Unexpected rank info, " ++ show t


