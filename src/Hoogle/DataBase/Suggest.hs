
module Hoogle.DataBase.Suggest where

import Control.Monad
import Data.Binary.Defer
import Data.Binary.Defer.Trie
import Data.Char
import Data.List
import qualified Data.Map as Map
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.Item
import Data.Generics.Uniplate


newtype Suggest = Suggest (Trie SuggestItem) deriving Show

data SuggestItem = SuggestItem
    {suggestCtor :: Maybe String -- constructor (and who the type is)
    ,suggestData :: [(String,Int)] -- data type, name (case correct), and possible kinds
    ,suggestClass :: [(String,Int)] -- class, name (case correct), kinds
    }
    deriving Show


instance BinaryDefer SuggestItem where
    put (SuggestItem a b c) = put a >> put b >> put c
    get = get3 SuggestItem

instance BinaryDefer Suggest where
    put (Suggest x) = put x
    get = get1 Suggest


createSuggest :: [(TextItem, Maybe Entry)] -> Suggest
createSuggest xs = Suggest $ newTrie $ Map.toList res
    where
        res = foldl f Map.empty $ concatMap (getTextItem . fst) xs
            where f m (s,i) = Map.insertWith join (map toLower s) i m

        join (SuggestItem a1 b1 c1) (SuggestItem a2 b2 c2) =
            SuggestItem (a1 `mplus` a2) (nub $ b1++b2) (nub $ c1++c2)

        sData  c n = (c, SuggestItem Nothing [(c,n)] [])
        sClass c n = (c, SuggestItem Nothing [] [(c,n)])

        getTextItem :: TextItem -> [(String,SuggestItem)]
        getTextItem (ItemClass x   ) = getTypeSig sClass x
        getTextItem (ItemFunc _ x  ) = getTypeSig sData x -- TODO: add ctors here
        getTextItem (ItemAlias x y ) = getTypeSig sData x ++ getTypeSig sData y
        getTextItem (ItemData _ x  ) = getTypeSig sData x
        getTextItem (ItemInstance x) = getTypeSig sClass x
        getTextItem _ = []

        getTypeSig typ (TypeSig x y) = concatMap (getType sClass) x ++ getType typ y

        getType typ x =
            [typ c (length ys) | TApp (TLit c) ys <- [x], c `notElem` [",","->"]] ++
            concatMap (getType sData) (children x)
