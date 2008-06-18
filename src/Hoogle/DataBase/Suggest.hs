
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


newtype Suggest = Suggest (Trie SuggestItem)

data SuggestItem = SuggestItem
    {suggestCtor :: Maybe String -- constructor (and who the type is)
    ,suggestData :: [(String,Int)] -- data type, name (case correct), and possible kinds
    ,suggestClass :: [(String,Int)] -- class, name (case correct), kinds
    }


instance Show Suggest where
    show (Suggest x) = show x

instance Show SuggestItem where
    show (SuggestItem a b c) = concat $ intersperse ", " $
        ["ctor " ++ x | Just x <- [a]] ++ f "data" b ++ f "class" c
        where
            f msg xs = [msg ++ " " ++ a ++ " " ++ show b | (a,b) <- xs]


instance BinaryDefer Suggest where
    put (Suggest x) = put x
    get = get1 Suggest

instance BinaryDefer SuggestItem where
    put (SuggestItem a b c) = put a >> put b >> put c
    get = get3 SuggestItem


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
        getTextItem (ItemFunc n x  ) = getTypeSig sData x ++ getCtor n x
        getTextItem (ItemAlias x y ) = getTypeSig sData x ++ getTypeSig sData y
        getTextItem (ItemData _ x  ) = getTypeSig sData x
        getTextItem (ItemInstance x) = getTypeSig sClass x
        getTextItem _ = []

        getTypeSig typ (TypeSig x y) = concatMap (getType sClass) x ++ getType typ y

        getType typ x =
            [typ c (length ys) | TApp (TLit c) ys <- [x]
                               , c /= "->" && not (isTLitTuple c)] ++
            concatMap (getType sData) (children x)

        getCtor name (TypeSig _ x) =
            [ (name, SuggestItem (Just c) [] [])
            | n:_ <- [name], isUpper n
            , (TLit c,_) <- [fromTApp x]]
