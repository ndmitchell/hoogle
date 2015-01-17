{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Output.Tags(Tags, writeTags, readTags, listTags, filterTags, pruneTags, searchTags) where

import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Data.Tuple.Extra
import Data.Maybe

import Input.Type
import Query
import General.Util

writeTags :: Database -> (String -> [(String,String)]) -> [(Maybe Id, Items)] -> IO ()
writeTags (Database file) extra xs = do
    writeFileBinary (file <.> "tags") $ unlines $ f [] [] (Id 0) xs
    where
        -- active groups currently scope over the thing
        -- next groups scope from the next identifier
        -- lst is the last identifier that closing groups scope up to
        f :: [((String, (String,String)),Id)] -> [(String, (String,String))] -> Id -> [(Maybe Id, Items)] -> [String]
        f active next lst ((i,x):xs) = case i of
            Nothing -> f active next2 lst xs
            Just i ->
                let (stop,cont) = partition (\x -> fst (fst x) `elem` map fst next2) active
                in f stop [] lst [] ++ f (cont ++ map (,i) next2) [] i xs
            where
                next2 = case g x of Nothing -> next; Just (a,b) -> map (a,) b ++ filter ((/=) a . fst) next
        f active next lst [] = [a ++ " " ++ b ++ " " ++ show c ++ " " ++ show lst | ((_,(a,b)),c) <- reverse active]

        g (IPackage x) = Just ("package",("package",x):extra x)
        g (IModule x) = Just ("module",[("module",x)])
        g _ = Nothing


newtype Tags = Tags [((String, String), (Id, Id))]


readTags :: Database -> IO Tags
readTags (Database file) = do
    x <- readFile' $ file <.> "tags"
    return $ Tags [((cat, unwords bod), (read i1, read i2))
        | x <- lines x, let cat:xs = words x, let ([i1,i2],bod) = both reverse $ splitAt 2 $ reverse xs]


listTags :: Tags -> [String]
listTags (Tags xs) = nub $ map (\(a,b) -> a ++ ":" ++ b) $ sortOn (f &&& second lower) $ filter ((/=) "module" . fst) $ map fst xs
    where
        f ("set",x) = fromMaybe 0.9 $ lookup x [("stackage",0.0),("haskell-platform",0.1)]
        f ("package",x) = 1
        f ("category",x) = 2
        f ("license",x) = 3
        f _ = 4

filterTags :: Tags -> [Scope] -> (Id -> Bool)
filterTags (Tags ts) qs = \i -> let g (lb,ub) = i >= lb && i <= ub in not (any g neg) && (null pos || any g pos)
    where (pos, neg) = both (map snd) $ partition fst $ concatMap f qs
          f (Scope sense cat val) = map ((,) sense . snd) $ filter ((==) (cat,val) . fst) ts

-- return Left ("module","Data.List") to say "See more results from Data.List" and start cutting them off
pruneTags :: Tags -> [Id] -> [Either (String,String) Id]
pruneTags _ = map Right


searchTags :: Tags -> [Scope] -> [(Score,Id)]
searchTags (Tags ts) qs = map ((0,) . fst . snd) $ filter (flip elem [(cat,val) | Scope True cat val <- qs] . fst) ts
