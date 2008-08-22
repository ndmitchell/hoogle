
module Hoogle.Operations.Rank(rank) where

import General.Code
import Hoogle.Query.All
import Hoogle.Search.All
import Hoogle.TextBase.All
import Hoogle.DataBase.All
import Hoogle.TypeSig.All
import Hoogle.Item.All
import Data.Binary.Defer.Index
import Hoogle.Operations.RankSolve

-- Privilaged imports
import Hoogle.DataBase.TypeSearch.Cost(Cost(..))
import Hoogle.DataBase.TypeSearch.TypeScore(costsTypeScore)
import Hoogle.Search.Result(Score(TypeScore))


data RankTest = RankTest TypeSig [TypeSig]
                deriving Show


rank :: FilePath -> IO ()
rank file = do
    (tb,tests) <- liftM readRankTests $ readFile file
    let rel = concatMap (runRankTest tb) tests
    rankSolve rel



readRankTests :: String -> (TextBase,[RankTest])
readRankTests xs = (tb, join $ concatMap parse rest)
    where
        (pre,rest) = break ("@" `isPrefixOf`) $ lines xs
        tb = right $ parseTextBaseString $ unlines pre

        parse :: String -> [(Bool, TypeSig)]
        parse xs | null xs || "--" `isPrefixOf` xs = []
        parse ('@':xs) = [(True,f xs)]
        parse xs = [(False,f xs)]

        f = right . parseTypeSig 
        right (Right x) = x
        right (Left x) = error $ "readRank failed to parse " ++ show x

        join ((_,t):xs) = RankTest t (map snd a) : join b
            where (a,b) = break fst xs
        join [] = []


runRankTest :: TextBase -> RankTest -> [Cmp]
runRankTest tb (RankTest t xs) = order $ map grab xs2
    where
        xs2 = zip ["f" ++ show i | i <- [1..]] xs

        res = searchAll [db] q
        db = createDataBase [] $ tb ++ [(ItemFunc a b, "") | (a,b) <- xs2]
        q = blankQuery{typeSig=Just t}

        grab :: (String,TypeSig) -> (TypeSig,[Cost])
        grab (name,typ) = (,) typ $
            headDef (err $ "Couldn't find result for " ++ show typ)
            [sort $ costsTypeScore c | r <- res, entryName (fromLink $ resultEntry r) == name
                                     , TypeScore c <- resultScore r]


        order ((at,a):xs@((bt,b):_))
            | isNothing match = (a :< b) : order xs
            | otherwise = err $ "Two items have the same score, " ++ show at ++
                                " AND " ++ show (fst $ fromJust match)
                where match = find ((==) a . snd) xs
        order _ = []

        err msg = error $ "Hoogle.Operations.runRankTest\nTest: " ++ show t ++ "\n" ++ msg
