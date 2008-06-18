
module Hoogle.DataBase.Items where

import Control.Monad.State
import Data.Maybe
import Data.Binary.Defer.Index
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.Item
import Data.Binary.Defer hiding (get,put)
import qualified Data.Binary.Defer as D
import Safe


data Items = Items
    {packages :: Index Package
    ,modules :: Index Module
    ,entries :: Index Entry
    }

instance BinaryDefer Items where
    put (Items a b c) = D.put a >> D.put b >> D.put c
    get = get3 Items

instance Show Items where
    show (Items a b c) = f "Packages" a ++ f "Modules" b ++ f "Entries" c
        where f header x = "== " ++ header ++ " ==\n\n" ++ show x ++ "\n\n"


-- temporary state structure
data S = S {pkg :: Package
           ,modId :: Int
           ,mods :: [Module]
           ,modCur :: Maybe (Lookup Module)
           ,entId :: Int
           ,ents :: [(TextItem, Maybe Entry)]
           }

createItems :: [TextItem] -> (Items, [(TextItem, Maybe Entry)])
createItems xs = unS $ execState (mapM f xs) s0
    where
        s0 = S (Package 0 "" "" "" "") 0 [] Nothing 0 []

        unS s = (Items (newIndex [pkg s])
                       (newIndex $ reverse $ mods s)
                       (newIndex $ reverse $ mapMaybe snd $ ents s)
                ,ents s)

        f :: TextItem -> State S ()
        f i@ItemInstance{} = addTextItem i

        f i@(ItemAttribute "keyword" name) = addEntry i False EntryKeyword
                [Keyword "keyword",Text " ",Focus name]

        f i@(ItemAttribute name val) = do
            addTextItem i
            when (name == "package") $ modify $ \s -> s{pkg = (pkg s){packageName = val}}
            when (name == "version") $ modify $ \s -> s{pkg = (pkg s){packageVersion = val}}

        f i@(ItemModule xs) = do
            s <- get
            let modI = modId s
                m = Module modI xs (newLookup 0)
            put s{modId = modI + 1, mods = m : mods s
                 ,modCur = Just $ newLookup modI}
            addEntry i True EntryModule
                [Keyword "module", Text $ ' ' : concatMap (++ ".") (init xs), Focus (last xs)]

        f i = addEntry i True EntryOther (render i)


        render (ItemClass i) = [Keyword "class", Text " "] ++ typeHead i
        render (ItemFunc name typ) = [Focus name, Text " :: ", Text (show typ)]
        render (ItemAlias a b) = [Keyword "type", Text " "] ++ typeHead a ++ [Text $ " = " ++ show b]
        render (ItemData d t) = [Keyword (show d), Text " "] ++ typeHead t

        typeHead (TypeSig con sig) = [Text $ showConstraint con, Focus a, Text b]
            where (a,b) = break (== ' ') $ show sig


        addTextItem i = modify $ \s -> s{ents = (i,Nothing) : ents s}

        addEntry i modu typ txt = do
            s <- get
            let entI = entId s
                e = Entry entI
                          (if modu then modCur s else Nothing)
                          (headDef "" [i | Focus i <- txt])
                          txt typ
            put $ s{entId = entI + 1, ents = (i, Just e) : ents s}
