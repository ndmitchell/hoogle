{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataTags(Tags, writeTags, readTags, filterTags, pruneTags) where

import System.IO.Extra
import Data.List.Extra
import System.FilePath

import Type


writeTags :: Database -> [(Maybe Id, Items)] -> IO ()
writeTags (Database file) xs = do
    writeFileBinary (file <.> "tags") $ unlines $ f [] [] (Id 0) xs
    where
        -- active groups currently scope over the thing
        -- next groups scope from the next identifier
        -- lst is the last identifier that closing groups scope up to
        f :: [((String, String),Id)] -> [(String, String)] -> Id -> [(Maybe Id, Items)] -> [String]
        f active next lst ((i,x):xs) = case i of
            Nothing -> f active next2 lst xs
            Just i ->
                let (stop,cont) = partition (\x -> fst (fst x) `elem` map fst next2) active
                in f stop [] lst [] ++ f (cont ++ map (,i) next2) [] i xs
            where
                next2 = case g x of Nothing -> next; Just (a,b) -> (a,b) : filter ((/=) a . fst) next
        f active next lst [] = [a ++ " " ++ b ++ " " ++ show c ++ " " ++ show lst | ((a,b),c) <- active]

        g (IPackage x) = Just ("package",x)
        g (IModule x) = Just ("module",x)
        g (ITag a b) = Just (a,b)
        g _ = Nothing


data Tags = Tags


readTags :: Database -> IO Tags
readTags _ = return Tags


filterTags :: Tags -> [QTag] -> (Id -> Bool)
filterTags _ _ _ = True

-- return Left ("module","Data.List") to say "See more results from Data.List" and start cutting them off
pruneTags :: Tags -> [Id] -> [Either (String,String) Id]
pruneTags _ = map Right
