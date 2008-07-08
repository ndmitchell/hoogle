
module Hoogle.DataBase.Items where

import Control.Monad.State
import Data.Binary.Defer.Index
import General.Code
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.Item
import Hoogle.DataBase.Haddock
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
    show (Items a b c) = f "Packages" a ++ f "Modules" b ++ f "Entries" c
        where f header x = "== " ++ header ++ " ==\n\n" ++ show x ++ "\n\n"


-- temporary state structure
data S = S {pkg :: Package
           ,modId :: Int
           ,mods :: [Module]
           ,modCur :: Maybe (Link Module)
           ,entId :: Int
           ,ents :: [(TextItem, Maybe Entry)]
           }

createItems :: [(TextItem,String)] -> (Items, [(TextItem, Maybe (Link Entry))])
createItems xs = res
    where
        res = unS $ execState (mapM (uncurry f) xs) s0
        s0 = S (Package "" "" "" "") 0 [] Nothing 0 []

        unS s = (Items (newIndex [pkg s])
                       ms
                       (newIndex $ mapMaybe (liftM fromLink . snd) esJ2)
                ,map (id *** const Nothing) esN ++ esJ2)
            where
                ms = newIndex $ reverse $ mods s
                (esJ,esN) = partition (isJust . snd) $ ents s
                esJ2 = zipWith (\i (ti,Just e) -> (ti,Just $ newLink i e)) [0..] $
                       sortOn (entryScore . fromJust . snd) esJ

        f :: TextItem -> String -> State S ()
        f i@ItemInstance{} _ = addTextItem i

        f i@(ItemAttribute "keyword" name) d = addEntry i False EntryKeyword d
                [Keyword "keyword",Text " ",Focus name]

        f i@(ItemAttribute name val) _ = do
            addTextItem i
            when (name == "package") $ modify $ \s -> s{pkg = (pkg s){packageName = val}}
            when (name == "version") $ modify $ \s -> s{pkg = (pkg s){packageVersion = val}}

        f i@(ItemModule xs) d = do
            s <- get
            let modI = modId s
                m = Module xs (newLink 0 $ lookupIndex (newLookup 0) (packages $ fst res))
            put s{modId = modI + 1, mods = m : mods s
                 ,modCur = Just $ newLink modI m}
            addEntry i True EntryModule d
                [Keyword "module", Text $ ' ' : concatMap (++ ".") (init xs), Focus (last xs)]

        f i d = addEntry i True EntryOther d (renderTextItem i)

        addTextItem i = modify $ \s -> s{ents = (i,Nothing) : ents s}

        addEntry i modu typ doc txt = do
            s <- get
            let entI = entId s
                e = Entry (if modu then modCur s else Nothing)
                          (headDef "" [i | Focus i <- txt])
                          txt typ (newHaddock doc)
            put $ s{entId = entI + 1, ents = (i, Just e) : ents s}
