{-# LANGUAGE RecordWildCards #-}

module Hoogle.DataBase.Items where

import Control.Monad.State
import Data.Binary.Defer.Index
import General.Code
import General.Web
import Hoogle.Type.All
import Data.Binary.Defer hiding (get,put)
import qualified Data.Binary.Defer as D

-- Invariant: Index Entry is by order of EntryScore

data Items = Items
    {packages :: Index Package
    ,modules :: Index Module
    ,entries :: Index Entry
    }

instance BinaryDefer Items where
    put (Items a b c) = put3 a b c
    get = do
        res@(Items a b c) <- get3 Items
        getDeferPut a
        getDeferPut b
        getDeferPut c
        return res


instance Show Items where
    show (Items a b c) = f "Packages" a ++ "\n" ++ f "Modules" b ++ "\n" ++ f "Entries" c
        where f header x = "== " ++ header ++ " ==\n\n" ++ show x


-- temporary state structure
data S a = S {count :: Int, values :: [a]}

newS = S (-1) []
newIndexS = newIndex . reverse . values
addS x (S i xs) = S (i+1) (x:xs)
getS (S i (x:xs)) = newLink i x
getS _ = error "DataBase.Items.getS, lacking a package/module?"


entriesItems :: Items -> [Link Entry]
entriesItems = indexLinks . entries


createItems :: [TextItem] -> Items
createItems xs = Items (newIndexS pkgs) (newIndexS mods)
                       (newIndex $ sortOn entryScore ents)
    where
        (ents, (pkgs,mods)) = flip runState (newS,newS) $ concatMapM addTextItem xs


-- add a TextItem to the state S
addTextItem :: TextItem -> State (S Package, S Module) [Entry]
addTextItem TextItem{..} = do
    when (itemLevel == 0) $
        modify $ \(ps,ms) -> (addS (Package (head itemName) itemURL) ps, ms)
    when (itemLevel == 1) $
        modify $ \(ps,ms) -> let p = getS ps in (ps, addS (Module itemName p (packageURL (fromLink p) `combineURL` itemURL)) ms)
    (ps,ms) <- get
    let p = getS ps
        m = if itemLevel > 1 then Just $ getS ms else Nothing
        url = if itemLevel == 1 then packageURL (fromLink p) `combineURL` itemURL
              else if itemLevel > 1 then moduleURL (fromLink $ fromJust m) `combineURL` itemURL
              else itemURL
    return [Entry m p
        (intercalate "." itemName)
        itemDisp
        (htmlDocumentation itemDocs)
        url
        itemType]


mergeItems :: [Items] -> Items
mergeItems [x] = x
mergeItems xs = Items
        (newIndex $ concat $ reverse ps)
        (newIndex $ concat $ reverse ms)
        (newIndex $ sortOn entryScore $ concat $ reverse es)
    where
        (_,ps,_,ms,_,es) = foldl' f (0,[],0,[],0,[]) xs

        f (pi,ps,mi,ms,ei,es) (Items p m e) =
                (pi+length p3,p3:ps, mi+length m3,m3:ms, ei+length e3,e3:es)
            where
                (p2,p3) = add pi p id
                (m2,m3) = add mi m id
                (_ ,e3) = add ei e $ \x -> x{entryModule = liftM (\x -> m2 !! linkKey x) $ entryModule x
                                            ,entryPackage = p2 !! linkKey (entryPackage x)}

                add i xs f = (zipWith newLink [i..] xs2, xs2)
                    where xs2 = map (f . fromLink) $ indexLinks xs
