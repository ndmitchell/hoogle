{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Hoogle.DataBase.Items(Items, createItems, entriesItems) where

import General.Base
import General.Web
import Hoogle.Type.All
import qualified Data.Map as Map
import Hoogle.Store.All

-- Invariant: items are by order of EntryScore
newtype Items = Items {fromItems :: Defer [Once Entry]}

instance NFData Items where
    rnf (Items a) = rnf a

entriesItems :: Items -> [Once Entry]
entriesItems = fromDefer . fromItems


instance Store Items where
    put (Items a) = put1 a
    get = get1 Items


instance Show Items where
    show (Items x) = "== Entries ==\n\n" ++ show x


instance Monoid Items where
    mempty = mergeItems []
    mappend x y = mergeItems [x,y]
    mconcat = mergeItems


createItems :: [TextItem] -> Items
createItems xs = mergeItems [Items $ Defer $ fs Nothing Nothing xs]
    where
        fs pkg mod [] = []
        fs pkg mod (x:xs) = r : fs pkg2 mod2 xs
            where r = f pkg2 mod2 x
                  pkg2 = if itemLevel x == 0 then Just r else pkg
                  mod2 = if itemLevel x == 1 then Just r else mod

        f pkg mod TextItem{..} = once $ Entry [(url, catMaybes [pkg,mod])] itemKind itemLevel itemName itemDisp
            (readDocsHTML itemDocs) itemPriority itemKey itemType
            where url | Just pkg <- pkg, itemLevel == 1 || (itemLevel > 1 && isNothing mod) = entryURL (fromOnce pkg) `combineURL` itemURL
                      | Just mod <- mod, itemLevel > 1 = entryURL (fromOnce mod) `combineURL` itemURL
                      | otherwise = itemURL


-- | Given a set of items, which may or may not individually satisfy the entryScore invariant,
--   make it so they _do_ satisfy the invariant.
--   Also merge any pair of items which are similar enough.
--
--   If something which is a parent gets merged, then it will still point into the database,
--   but it won't be very useful.
mergeItems :: [Items] -> Items
mergeItems = Items . Defer . sortOn (entryScore . fromOnce) . Map.elems . foldl' add Map.empty . concatMap entriesItems
    where
        add mp x = Map.insertWith (\x1 x2 -> once $ entryJoin (fromOnce x1) (fromOnce x2))
                                  (entryUnique $ fromOnce x) x mp
