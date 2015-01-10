{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataTags(writeGroups, lookupAttribute, lookupModule) where

-- grp = 1.28Mb
-- wrd = 10.7Mb

-- [(".grp",1_343_808),(".ids",247_336_269),(".wrd",11_230_952)]
-- [(".grp",1_314_256),(".ids",244_154_208),(".wrd",7_369_220)]


import System.IO.Extra
import Data.List.Extra
import System.FilePath

import Type


writeGroups :: FilePath -> [(Maybe Id, Items)] -> IO ()
writeGroups file xs = do
    writeFileBinary (file <.> "groups") $ unlines $ f [] [] (Id 0) xs
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


lookupAttribute :: FilePath -> String -> String -> IO [(Int, Int)]
lookupAttribute = undefined

lookupModule :: FilePath -> String -> IO [(Int,Int)]
lookupModule = undefined
