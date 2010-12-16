{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Hoogle.DataBase.Items(Items, createItems, entriesItems) where

import Data.Binary.Defer.Index
import General.Base
import General.Util
import General.Web
import Hoogle.Type.All
import qualified Data.Map as Map
import Data.Binary.Defer hiding (get,put)
import qualified Data.Binary.Defer as D

-- Invariant: Index Entry is by order of EntryScore
newtype Items = Items (Index Entry)

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

        f pkg mod TextItem{..} = Entry pkg mod itemName itemDisp
            (htmlDocumentation itemDocs) url itemPriority itemKey itemType
            where url | Just pkg <- pkg, itemLevel == 1 || (itemLevel > 1 && isNothing mod) = entryURL (fromLink pkg) `combineURL` itemURL
                      | Just mod <- mod, itemLevel > 1 = entryURL (fromLink mod) `combineURL` itemURL
                      | otherwise = itemURL


-- | Given a set of items, which may or may not individually satisfy the entryScore invariant,
--   make it so they _do_ satisfy the invariant
mergeItems :: [Items] -> Items
mergeItems xs = Items $ newIndex $ map ren ijv
    where
        -- xs are sets of items, vs are sets of values, is index xs, js index vs
        mp = Map.fromList [(ij, newLink n v) | (n,(ij,v)) <- zip [0..] ijv]
        ijv = sortOn (entryScore . snd) [((i,linkKey jv),fromLink jv) | (i,Items vs) <- zip [0..] xs, jv <- indexLinks vs]

        ren (ij,v) = v{entryPackage = f ij $ entryPackage v, entryModule = f ij $ entryModule v}
        f _ Nothing = Nothing
        f (i,j) (Just e) = Just $ mp Map.! (i,linkKey e) 
