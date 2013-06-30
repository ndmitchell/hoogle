{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.DataBase.Suggest(Suggest, createSuggest, askSuggest) where

import General.Base
import General.Util
import Hoogle.Store.All
import qualified Data.Map as Map
import Hoogle.Type.All
import Data.Generics.Uniplate


newtype Suggest = Suggest {fromSuggest :: Map.Map String SuggestItem}

-- if something is both a data and a ctor, no need to mention the ctor
data SuggestItem = SuggestItem
    {suggestCtor :: Maybe String -- constructor (and who the type is)
    ,suggestData :: [(String,Int)] -- data type, name (case correct), and possible kinds
    ,suggestClass :: [(String,Int)] -- class, name (case correct), kinds
    }
    deriving Typeable


instance NFData Suggest where
    rnf (Suggest a) = rnf a

instance NFData SuggestItem where
    rnf (SuggestItem a b c) = rnf (a,b,c)

instance Show Suggest where
    show (Suggest x) = show x

instance Show SuggestItem where
    show (SuggestItem a b c) = intercalate ", " $
        ["ctor " ++ x | Just x <- [a]] ++ f "data" b ++ f "class" c
        where
            f msg xs = [msg ++ " " ++ a ++ " " ++ show b | (a,b) <- xs]


instance Store Suggest where
    put (Suggest x) = put x
    get = get1 Suggest

instance Store SuggestItem where
    put (SuggestItem a b c) = put3 a b c
    get = get3 SuggestItem

instance Monoid Suggest where
    mempty = mergeSuggest []
    mappend x y = mergeSuggest [x,y]
    mconcat = mergeSuggest

-- note: do not look inside class's for data type information
--       as they may have higher-kinds and get it wrong
createSuggest :: [Suggest] -> [Fact] -> Suggest
createSuggest deps xs = mergeSuggest (s:deps)
    where
        s = Suggest res
        res = foldl f Map.empty $ concatMap getTextItem xs
            where f m (s,i) = Map.insertWith joinItem (map toLower s) i m

        sData  c n = (c, SuggestItem Nothing [(c,n)] [])
        sClass c n = (c, SuggestItem Nothing [] [(c,n)])

        getTextItem :: Fact -> [(String,SuggestItem)]
        getTextItem (FactDataKind a b) = [sData a b]
        getTextItem (FactClassKind a b) = [sClass a b]
        getTextItem (FactCtorType a b) = [(a, SuggestItem (Just b) [] [])]
        getTextItem _ = []


mergeSuggest :: [Suggest] -> Suggest
mergeSuggest = Suggest . Map.unionsWith joinItem . map fromSuggest


joinItem :: SuggestItem -> SuggestItem -> SuggestItem
joinItem (SuggestItem a1 b1 c1) (SuggestItem a2 b2 c2) =
    SuggestItem
        (if null b1 && null b2 then a1 `mplus` a2 else Nothing)
        (f b1 b2) (f c1 c2)
    where
        f x y = map (second maximum) $ sortGroupFsts $ x ++ y


askSuggest :: [Suggest] -> TypeSig -> Maybe (Either String TypeSig)
askSuggest sug q@(TypeSig con typ)
        | q2 /= q = Just (Right q2)
        | not $ null datas = unknown "type" datas
        | not $ null classes = unknown "class" classes
        | otherwise = Nothing
    where
        tries = map fromSuggest sug
        get x = case mapMaybe (Map.lookup $ map toLower x) tries of
                    [] -> Nothing
                    xs -> Just $ foldr1 joinItem xs

        con2 = map (improve get True) con
        typ2 = improve get False typ
        q2 = contextTrim $ insertVars $ TypeSig con2 typ2
        insertVars = transformSig (\x -> if x == TVar "" then TVar var else x)
        var = head $ filter (/= "") $ variables typ2 ++ concatMap variables con2 ++ ["a"]

        -- figure out if you have a totally unknown thing --
        classes = [x | c <- con, (TLit x,_) <- [fromTApp c], bad True x]
        datas = [x | TLit x <- concatMap universe $ typ : concatMap (snd . fromTApp) con
                   , not $ isTLitTuple x, bad False x]
        unknown typ (x:_) = Just $ Left $ "Warning: Unknown " ++ typ ++ " " ++ x

        bad cls name = case get name of
            Nothing -> True
            Just i | cls -> null $ suggestClass i
                   | otherwise -> null (suggestData i) && isNothing (suggestCtor i)


-- remove context which doesn't reference variables in the RHS
contextTrim :: TypeSig -> TypeSig
contextTrim (TypeSig con typ) = TypeSig (filter (not . bad) con) typ
    where var = variables typ
          bad x = isTVar (fst $ fromTApp x) || null (variables x `intersect` var)


improve :: (String -> Maybe SuggestItem) -> Bool -> Type -> Type
improve get cls typ
        | not cls = f $ transform (improveName nameTyp) typ
        | otherwise = improveArity arity $
            tApp (improveName nameCls t1) (map (transform (improveName nameTyp)) ts)
    where
        (t1,ts) = fromTApp typ
        nameTyp = maybe [] (\x -> maybeToList (suggestCtor x) ++ map fst (suggestData x)) . get
        nameCls = maybe [] (map fst . suggestClass) . get
        
        arity x = lookup x . (if cls then suggestClass else suggestData) =<< get x
          
        f x = case improveArity arity x of
                   TApp x xs -> TApp x (map f xs)
                   x -> descend f x


-- Given a name, return its arity
improveArity :: (String -> Maybe Int) -> Type -> Type
improveArity f o = case fromTApp o of
    (TLit x, xs) ->
        case f x of
            Just i -> tApp (TLit x) $ take i $ xs ++ repeat (TVar "")
            _ -> o
    _ -> o


-- Given a name, return the names it could possibly be
improveName :: (String -> [String]) -> Type -> Type
improveName f (TLit x) | ys /= [] && x `notElem` ys = TLit (head ys)
    where ys = f x
improveName f (TVar x) | length x > 1 && ys /= [] = TLit (head ys)
    where ys = f x
improveName f x = x
