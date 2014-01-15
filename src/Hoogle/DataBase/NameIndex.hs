{-# LANGUAGE RecordWildCards #-}

{- |
Write out:
foo.hoo.str - heirarchical format, {base{Data{List{map filter str}}}}
foo.hoo.idx - array of positions, each item being {len {item name, item url} documentation}
-}

module Hoogle.DataBase.NameIndex(saveNameIndex) where

import General.Base
import Data.Array
import Hoogle.DataBase.Type
import Hoogle.Type.All
import Hoogle.Store.All


saveNameIndex :: FilePath -> DataBase -> IO ()
saveNameIndex out db = do
    let ents = entriesItems $ items db
        (idx, str) = flattenPaths [(e,p) | e <- ents, p <- entryPaths $ fromOnce e]
    writeFileUtf8 (out ++ ".str") $ unlines str
    runSPut  (out ++ ".idx") $ put $ listArray (0, length idx - 1) idx
    writeFileUtf8 (out ++ ".typ") $ unlines [entryName e ++ " :: " ++ show t | e <- map fromOnce ents, Just t <- [entryType e]]

entryPaths :: Entry -> [[String]]
entryPaths x = [map (entryName . fromOnce) loc ++ [entryName x | entryLevel x > 1] | (_,loc) <- entryLocations x]

flattenPaths :: Show a => [(a, [String])] -> ([a], [String])
flattenPaths = (concat *** concat) . unzip . map f . groupBy ((==) `on` fst) . sortBy (compare `on` fst) . map down
    where
        down (a,b) = (head b, (a, tail b))

        f xs = case partition (null . snd) $ map snd xs of
            (nows, []) -> (map fst nows, replicate (length nows) $ escape $ fst $ head xs)
            ([(now, _)], later) -> let (a,b) = flattenPaths later in (now : a, ('{':escape (fst $ head xs)) : b ++ ["}"])
            (as, bs) -> error $ show $ "Failure when generating, not a single root element: " ++ show (fst $ head xs, length as, length bs)

        escape ('{':xs) = '{':'{':xs
        escape ('}':xs) = '}':'}':xs
        escape xs = xs
