
module Hoogle.DataBase.Items where

import Control.Monad.State
import Data.Binary.Defer.Index
import General.Code
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.Item.All
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
data S = S {pkg :: Package
           ,modId :: Int
           ,mods :: [Module]
           ,modCur :: Maybe (Link Module)
           }


entriesItems :: Items -> [Link Entry]
entriesItems = indexLinks . entries


createItems :: [(TextItem,String)] -> Items
createItems xs = Items (newIndex [pkg s])
                       (newIndex $ reverse $ mods s)
                       (newIndex $ sortOn entryScore ents)
    where
        -- circular programming to get pkg correct
        (ents, s) = flip runState s0 $ concatMapM (uncurry $ addTextItem (newLink 0 $ pkg s)) xs
        s0 = S (Package "" "" "" "") 0 [] Nothing


-- add a TextItem to the state S
addTextItem :: Link Package -> TextItem -> String -> State S [Entry]
addTextItem linkPkg ti doc = case ti of
    ItemInstance{} -> return []

    ItemAttribute "keyword" name -> add False EntryKeyword [Keyword "keyword",Text " ",Focus name]

    ItemAttribute name val -> do
        when (name == "package") $ modify $ \s -> s{pkg = (pkg s){packageName = val}}
        when (name == "version") $ modify $ \s -> s{pkg = (pkg s){packageVersion = val}}
        return []

    ItemModule xs -> do
        let m = Module xs linkPkg
        modId <- gets modId
        modify $ \s -> s{modId = modId + 1, mods = m : mods s
                        ,modCur = Just $ newLink modId m}
        add True EntryModule [Keyword "module", Text $ ' ' : concatMap (++ ".") (init xs), Focus (last xs)]

    _ -> add True EntryOther (renderTextItem ti)

    where
        add modu typ txt = do
            s <- get
            let sig = case ti of ItemFunc _ s -> Just (Defer s); _ -> Nothing
            return [Entry (if modu then modCur s else Nothing)
                          (headDef "" [i | Focus i <- txt])
                          txt typ (newHaddock doc) sig]


mergeItems :: [Items] -> Items
mergeItems [x] = x
mergeItems xs = Items
        (newIndex $ concat $ reverse ps)
        (newIndex $ concat $ reverse ms)
        (newIndex $ sortOn entryScore $ concat $ reverse es)
    where
        (pi,ps,mi,ms,ei,es) = foldl' f (0,[],0,[],0,[]) xs

        f (pi,ps,mi,ms,ei,es) (Items p m e) =
                (pi+length p3,p3:ps, mi+length m3,m3:ms, ei+length e3,e3:es)
            where
                (p2,p3) = add pi p id
                (m2,m3) = add mi m $ \x -> x{modulePackage = p2 !! linkKey (modulePackage x)}
                (e2,e3) = add ei e $ \x -> x{entryModule = liftM (\x -> m2 !! linkKey x) $ entryModule x}

                add i xs f = (zipWith newLink [i..] xs2, xs2)
                    where xs2 = map (f . fromLink) $ indexLinks xs
