
module Hoogle.Search.All where

import Data.Maybe
import Data.List
import Control.Monad

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Common.All
import Hoogle.TypeSig.All


-- return all the results
searchAll :: [DataBase] -> Query -> IO [Result DataBase]
searchAll databases query = getResults databases query


-- should be possible to fast-path certain searches, currently not done
searchRange :: [DataBase] -> Query -> Int -> Int -> IO [Result DataBase]
searchRange databases query from to = do
    res <- getResults databases query
    return $ take to $ drop from res


getResults :: [DataBase] -> Query -> IO [Result DataBase]
getResults databases query | not (null $ names query) = performTextSearch databases (head $ names query)
                           | isJust (typeSig query) = performTypeSearch databases (fromJust $ typeSig query)
        

performTextSearch :: [DataBase] -> String -> IO [Result DataBase]
performTextSearch databases query = do
        res <- concatMapM (`searchName` query) databases
        res <- return $ map head $ groupBy eqItemId $ sortBy cmpItemId res
        res <- mapM (\x -> loadResultItem x >>= loadResultModule) res
        return $ sortBy priority $ map fixupTextMatch res
    where
        cmpItemId x y = getItemId x `compare` getItemId y
        eqItemId x y = getItemId x == getItemId y
        getItemId = fromJust . itemId . itemResult

        nquery = length query
        fixupTextMatch (Result (Just txt) typ item) = Result (Just $ TextMatch loc (nname-nquery) badCase) typ item
            where
                loc = textLoc txt
                name = fromJust $ itemName item
                nname = length name
                badCase = length $ filter id $ zipWith (/=) query (drop loc name)

        priority x y = getStatus x `compare` getStatus y
        getStatus (Result (Just txt) typ item) =
            (textElse txt
            ,textCase txt
            ,itemPriority $ itemRest $ item
            ,fromJust (itemName item))


performTypeSearch :: [DataBase] -> TypeSig -> IO [Result DataBase]
performTypeSearch databases query = do
        res <- concatMapM (`searchType` query) databases
        res <- return $ concat $ sortBy cmpResults res
        res <- mapM (\x -> loadResultItem x >>= loadResultModule) res
        return res
    where
        cmpResults xs ys = f xs `compare` f ys
            where
                f = length . typeDiff . fromJust . typeResult . head


concatMapM f x = liftM concat $ mapM f x
