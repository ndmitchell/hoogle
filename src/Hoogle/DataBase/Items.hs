{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Hoogle.DataBase.Items(Items, createItems, entriesItems) where

import Hoogle.Store.Index
import General.Base
import General.Util
import General.Web
import Hoogle.Type.All
import qualified Data.Map as Map
import Hoogle.Store.All hiding (get,put)
import qualified Hoogle.Store.All as D

-- Invariant: Index Entry is by order of EntryScore
newtype Items = Items {fromItems :: Index Entry}

entriesItems :: Items -> [Link Entry]
entriesItems (Items x) = indexLinks x


instance BinaryDefer Items where
    put (Items a) = put1 a
    get = get1 Items


instance Show Items where
    show (Items x) = "== Entries ==\n\n" ++ show x


instance Monoid Items where
    mempty = mergeItems []
    mappend x y = mergeItems [x,y]
    mconcat = mergeItems


createItems :: [TextItem] -> Items
createItems xs = mergeItems [Items $ newIndex $ fs Nothing Nothing $ zip [0..] xs]
    where
        fs pkg mod [] = []
        fs pkg mod ((i,x):xs) = r : fs pkg2 mod2 xs
            where r = f pkg2 mod2 x
                  pkg2 = if itemLevel x == 0 then Just $ newLink i r else pkg
                  mod2 = if itemLevel x == 1 then Just $ newLink i r else mod

        f pkg mod TextItem{..} = Entry [(url, catMaybes [pkg,mod])] itemName itemDisp
            (htmlDocumentation itemDocs) itemPriority itemKey itemType
            where url | Just pkg <- pkg, itemLevel == 1 || (itemLevel > 1 && isNothing mod) = entryURL (fromLink pkg) `combineURL` itemURL
                      | Just mod <- mod, itemLevel > 1 = entryURL (fromLink mod) `combineURL` itemURL
                      | otherwise = itemURL


-- | Given a set of items, which may or may not individually satisfy the entryScore invariant,
--   make it so they _do_ satisfy the invariant.
--   Also merge any pair of items which are similar enough.
mergeItems :: [Items] -> Items
mergeItems xs = Items $ newIndex $ map (reindex (mp Map.!) . snd) ys
    where
        mp = Map.fromList [(i, newLink n y) | (n,(is, y)) <- zip [0..] ys, i <- is]
        ys = reorder $ flatten $ map (map (linkKey &&& fromLink) . indexLinks . fromItems) xs


reorder :: [(a,Entry)] -> [([a],Entry)]
reorder = sortOn (entryScore . snd) . Map.elems . foldl' f Map.empty
    where
        f mp (i,e@Entry{..}) = Map.insertWith g key ([i],e) mp
            where key = (entryName, entryText, entryDocs, entryKey, entryType)
        g (i1,e1) (i2,e2) = (i1++i2, e1
            {entryPriority=min (entryPriority e1) (entryPriority e2)
            ,entryLocations=nub $ concatMap entryLocations $ if entryScore e1 < entryScore e2 then [e1,e2] else [e2,e1]})


flatten :: [[(Int,Entry)]] -> [(Int,Entry)]
flatten xs = concat $ zipWith f ns xs
    where
        ns = 0 : scanl1 (+) (map length xs)
        f n x = [(i+n, reindex (\j -> newLink (j+n) (snd $ x!!j)) y) | (i,y) <- x]


reindex :: (Int -> Link Entry) -> Entry -> Entry
reindex op x = x{entryLocations = map (second $ map $ op . linkKey) $ entryLocations x}
