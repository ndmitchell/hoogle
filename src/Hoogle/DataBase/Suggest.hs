
module Hoogle.DataBase.Suggest where

import Control.Monad
import Data.Binary.Defer
import Data.Binary.Defer.Trie
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Hoogle.DataBase.Item
import Data.Generics.Uniplate


newtype Suggest = Suggest {fromSuggest :: Trie SuggestItem}

-- if something is both a data and a ctor, no need to mention the ctor
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


-- note: do not look inside class's for data type information
--       as they may have higher-kinds and get it wrong
createSuggest :: [(TextItem, Maybe Entry)] -> Suggest
createSuggest xs = Suggest $ newTrie $ Map.toList res
    where
        res = foldl f Map.empty $ concatMap (getTextItem . fst) xs
            where f m (s,i) = Map.insertWith joinItem (map toLower s) i m

        sData  c n = (c, SuggestItem Nothing [(c,n)] [])
        sClass c n = (c, SuggestItem Nothing [] [(c,n)])

        getTextItem :: TextItem -> [(String,SuggestItem)]
        getTextItem (ItemClass x   ) = getTypeSig True x
        getTextItem (ItemFunc n x  ) = getTypeSig False x ++ getCtor n x
        getTextItem (ItemAlias x y ) = getTypeSig False x ++ getTypeSig False y
        getTextItem (ItemData _ x  ) = getTypeSig False x
        getTextItem (ItemInstance x) = getTypeSig True x
        getTextItem _ = []

        getTypeSig cls (TypeSig x y) = concatMap (getType True) x ++ getType cls y

        getType cls (TApp (TLit c) ys) = add cls c (length ys) ++
                                         if cls then [] else concatMap (getType False) ys
        getType cls (TLit c) = add cls c 0
        getType cls x = if cls then [] else concatMap (getType False) $ children x

        add cls c i = [(if cls then sClass else sData) c i | not (isTLitTuple c)]

        getCtor name (TypeSig _ x) =
            [ (name, SuggestItem (Just c) [] [])
            | n:_ <- [name], isUpper n
            , (TLit c,_) <- [fromTApp $ last $ fromTFun x]]


joinItem :: SuggestItem -> SuggestItem -> SuggestItem
joinItem (SuggestItem a1 b1 c1) (SuggestItem a2 b2 c2) =
    SuggestItem
        (if null b1 && null b2 then a1 `mplus` a2 else Nothing)
        (nub $ b1++b2) (nub $ c1++c2)


askSuggest :: [Suggest] -> TypeSig -> Maybe (Either String TypeSig)
askSuggest sug q@(TypeSig con typ)
        | typ2 /= typ = Just (Right $ TypeSig con typ2)
        | not $ null datas = unknown "type" datas
        | not $ null classes = unknown "class" classes
        | otherwise = Nothing
    where
        tries = map fromSuggest sug
        get x = case catMaybes $ map (lookupTrie $ map toLower x) tries of
                    [] -> Nothing
                    xs -> Just $ foldr1 joinItem xs

        -- figure out if you have a totally unknown thing --
        classes = [x | c <- con, (TLit x,_) <- [fromTApp c], bad True x]
        datas = [x | TLit x <- concatMap universe $ typ : concatMap (snd . fromTApp) con, bad False x]
        unknown typ (x:_) = Just $ Left $ "Warning: Unknown " ++ typ ++ " " ++ x

        bad cls name = case get name of
            Nothing -> True
            Just i | cls -> null $ suggestClass i
                   | otherwise -> null (suggestData i) && isNothing (suggestCtor i)

        -- try and improve the type --
        typ2 = removeTApp $ transform f $ insertTApp typ
            where
                free = map (:[]) ['a'..] \\ [x | TVar x <- universe typ]

                f (TVar x) | isJust m && not (null d) = TLit $ fst $ head d
                    where m@ ~(Just SuggestItem{suggestData=d}) = get x

                f (TLit x) | isJust m && x `notElem` (map fst d) &&
                             (not (null d) || isJust c)
                           = if isJust c then TLit $ fromJust c
                             else TLit $ fst $ head $ d
                    where m@ ~(Just SuggestItem{suggestData=d, suggestCtor=c}) = get x

                f x = x
