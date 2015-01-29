{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Output.Tags(Tags, writeTags, readTags, listTags, filterTags, pruneTags, searchTags) where

import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Data.Tuple.Extra
import Data.Maybe
import qualified Data.Map as Map

import Input.Type
import Query
import General.Util

-- matches (a,b) if i >= a && i <= b
-- equal (is the thing in question) if i == a
-- .pkgs: shake 1002-1048 author:Neil-Mitchell license:GPL
-- .mods: Data.Shake.Binary 1034-1934 90890-89787 780-897


writeTags :: Database -> (String -> [(String,String)]) -> [(Maybe Id, Item)] -> IO ()
writeTags (Database file) extra xs = do
    let pkgs = splitIPackage xs
    writeFileBinary (file <.> "pkgs") $ f extra      $ pkgs
    writeFileBinary (file <.> "mods") $ f (const []) $ concatMap (splitIModule . snd) pkgs
    where
        f :: (String -> [(String,String)]) -> [(String,[(Maybe Id,a)])] -> String
        f extra xs = unlines [unwords $ x : ps ++ map (joinPair ":") (extra x) | (x,ps) <- Map.toAscList mp]
            where mp = Map.fromListWith (++) [ (s,[show (minimum is) ++ "-" ++ show (maximum is)])
                                             | (s,mapMaybe fst -> is) <- xs, s /= "", is /= []]


newtype Tags = Tags [((String, String), (Id, Id))]


readTags :: Database -> IO Tags
readTags = memoIO1 $ \(Database file) -> do
    pkgs <- readFile' $ file <.> "pkgs"
    mods <- readFile' $ file <.> "mods"
    return $ Tags $
        [(x, readIds range) | name:range:tags <- map words $ lines pkgs, x <- ("package",name) : map (splitPair ":") tags] ++
        [(("module",name), readIds r) | name:ranges <- map words $ lines mods, r <- ranges]
    where
        readIds = both read . splitPair "-"


listTags :: Tags -> [String]
listTags (Tags xs) = map head $ group $ map (\(a,b) -> a ++ ":" ++ b) $ sortOn (f &&& second lower) $ filter ((/=) "module" . fst) $ map fst xs
    where
        f ("set",x) = fromMaybe 0.9 $ lookup x [("stackage",0.0),("haskell-platform",0.1)]
        f ("package",x) = 1
        f ("category",x) = 2
        f ("license",x) = 3
        f _ = 4

filterTags :: Tags -> [Scope] -> (Id -> Bool)
filterTags ts qs = let fs = map (filterTags2 ts . snd) $ groupSort $ map (scopeCategory &&& id) qs in \i -> all ($ i) fs

filterTags2 (Tags ts) qs = \i -> let g (lb,ub) = i >= lb && i <= ub in not (any g neg) && (null pos || any g pos)
    where (pos, neg) = both (map snd) $ partition fst $ concatMap f qs
          f (Scope sense cat val) = map ((,) (sense) . snd) $ filter ((==) (cat,val) . fst) ts

-- return Left ("module","Data.List") to say "See more results from Data.List" and start cutting them off
pruneTags :: Tags -> [Id] -> [Either (String,String) Id]
pruneTags _ = map Right


searchTags :: Tags -> [Scope] -> [(Score,Id)]
searchTags (Tags ts) qs = map ((0,) . fst . snd) $ filter (flip elem [(cat,val) | Scope True cat val <- qs] . fst) ts
