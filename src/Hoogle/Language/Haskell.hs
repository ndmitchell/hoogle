{-# LANGUAGE PatternGuards #-}

module Hoogle.Language.Haskell(parseInputHaskell) where

import General.Base
import General.Util
import System.FilePath
import Hoogle.Type.All
import Language.Haskell.Exts.Annotated hiding (TypeSig,Type)
import qualified Language.Haskell.Exts.Annotated as HSE
import Data.Generics.Uniplate.Data


type S = SrcSpanInfo


parseInputHaskell :: HackageURL -> String -> ([ParseError], Input)
parseInputHaskell hackage = join . f [] "" . zip [1..] . lines
    where
        f com url [] = []
        f com url ((i,s):is)
            | "-- | " `isPrefixOf` s = f [drop 5 s] url is
            | "--" `isPrefixOf` s = f ([dropWhile isSpace $ drop 2 s | com /= []] ++ com) url is
            | "@url " `isPrefixOf` s =  f com (drop 5 s) is
            | all isSpace s = f [] "" is
            | otherwise = (case parseLine hackage i s of
                               Left y -> Left y
                               Right (as,bs) -> Right (as,[b{itemURL=if null url then itemURL b else url, itemDocs=unlines $ reverse com} | b <- bs]))
                          : f [] "" is

        join xs = (err, (concat as, ripple setPriority $ ripple setModuleURL $ concat bs))
            where (err,items) = unzipEithers xs
                  (as,bs) = unzip items


parseLine :: HackageURL -> Int -> String -> Either ParseError ([Fact],[TextItem])
parseLine _ line x | "(##)" `isPrefixOf` x = Left $ parseErrorWith line 1 "Skipping due to HSE bug #206" "(##)"
parseLine url line ('@':str) = case a of
        "entry" | b <- words b, b /= [] -> Right $ itemEntry b
        "package" | [b] <- words b, b /= "" -> Right $ itemPackage url b
        _ -> Left $ parseErrorWith line 2 ("Unknown attribute: " ++ a) $ '@':str
    where (a,b) = break isSpace str
parseLine _ line x | ["module",a] <- words x = Right $ itemModule $ split '.' a
parseLine _ line x
    | not continue = res
    | otherwise = fromMaybe res $ fmap Right $ parseTuple x `mappend` parseCtor x
    where (continue,res) = parseFunction line x

parseFunction line x = case parseDeclWithMode defaultParseMode{extensions=exts} $ x ++ ex of
    ParseOk y -> (,) False $ maybe (Left $ parseErrorWith line 1 "Can't translate" $ x ++ ex) Right $ transDecl x y
    ParseFailed pos msg -> (,) True $ Left $ parseErrorWith line (srcColumn pos) msg $ x ++ ex
    where ex = if "newtype " `isPrefixOf` x then " = N T" else " " -- space to work around HSE bug #205

parseTuple o@('(':xs) | ")" `isPrefixOf` rest
    , ParseOk y <- parseDeclWithMode defaultParseMode{extensions=exts} $ replicate (length com + 2) 'a' ++ drop 1 rest
    = transDecl o $ f y
    where
        (com,rest) = span (== ',') xs
        f (HSE.TypeSig sl [Ident sl2 _] ty) = HSE.TypeSig sl [Ident sl2 $ '(':com++")"] ty
parseTuple _ = Nothing

parseCtor x = case parseDeclWithMode defaultParseMode{extensions=exts} $ "data Data where " ++ x of
        ParseOk y -> transDecl x $ fmap (subtractCols 16) y
        _ -> Nothing

exts = map EnableExtension
    [EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
    ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples]


subtractCols :: Int -> SrcSpanInfo -> SrcSpanInfo
subtractCols n (SrcSpanInfo x xs) = SrcSpanInfo (f x) (map f xs)
    where f x = x{srcSpanStartColumn=srcSpanStartColumn x - n, srcSpanEndColumn=srcSpanEndColumn x - n}


textItem = TextItem 2 UnclassifiedItem "" "" Nothing (Str "") "" "" 0

fact x y = (x,[y])

itemPackage hackageUrl x = fact [] $ textItem{itemLevel=0, itemKey="", itemName=x,
    itemKind=PackageItem,
    itemURL= hackageUrl ++ "package/" ++ x ++ "/",
    itemDisp=Tags [emph "package",space,bold x]}

itemEntry (x:xs) = fact [] $ textItem{itemName=y, itemKey=y,
    itemDisp= if null xs then bold x else Tags [emph x,space,bold y]}
    where y = if null xs then x else unwords xs

itemModule xs = fact [] $ textItem{itemLevel=1, itemKey=last xs, itemName=intercalate "." xs,
    itemURL="", itemKind=ModuleItem,
    itemDisp=Tags [emph "module",Str $ " " ++ concatMap (++".") (init xs),bold $ last xs]}


-- apply things that need to ripple down, priorities and module URL's
ripple :: (Maybe TextItem -> Maybe TextItem -> TextItem -> TextItem) -> [TextItem] -> [TextItem]
ripple f = fs Nothing Nothing
    where
        fs a b [] = []
        fs a b (x:xs) = f a2 b2 x : fs a2 b2 xs
            where a2 = if itemLevel x == 0 then Just x else a
                  b2 = if itemLevel x == 1 then Just x else b


-- base::Prelude is priority 0
-- base, but not inside GHC is priority 1
-- Everything else is priority 2
setPriority pkg mod x = x{itemPriority = pri}
    where pri = if pkg2 == "base" && not ("GHC." `isPrefixOf` mod2) then (if mod2 == "Prelude" then 0 else 1) else 2
          mod2 = maybe "" itemName mod
          pkg2 = maybe "" itemName pkg


setModuleURL (Just pkg) _ x | itemLevel x == 1 = x{itemURL=if null $ itemURL x then f $ itemName x else itemURL x}
    where f xs = if "http://hackage.haskell.org/package/" `isPrefixOf` itemURL pkg
                 then "http://hackage.haskell.org/packages/archive/" ++ itemName pkg ++ "/latest/doc/html/" ++ file
                 else takeDirectory (itemURL pkg) ++ "/" ++ file
              where file = reps '.' '-' xs ++ ".html"
setModuleURL _ _ x = x


---------------------------------------------------------------------
-- TRANSLATE THINGS


transDecl :: String -> Decl S -> Maybe ([Fact],[TextItem])
transDecl x (GDataDecl s dat ctxt hd _ [] _) = transDecl x $ DataDecl s dat ctxt hd [] Nothing
transDecl x (GDataDecl _ _ _ _ _ [GadtDecl s name _ ty] _) = transDecl x $ HSE.TypeSig s [name] ty

transDecl x (HSE.TypeSig _ [name] tyy) = Just $ fact (ctr++kinds False typ) $ textItem{itemName=nam,itemKey=nam,
    itemType=Just typ, itemKind=kind,
    itemURL="#v:" ++ esc nam,
    itemDisp=formatTags x $ (cols snam,TagBold) : zipWith (\i a -> (cols a,TagColor i)) [1..] as ++ [(cols b,TagColor 0)]}
    where (snam,nam) = findName name
          (as,b) = initLast $ typeArgsPos tyy
          ctr = [FactCtorType nam y | ctorStart $ head nam, TLit y <- [fst $ fromTApp $ last $ fromTFun ty]]
          typ@(TypeSig _ ty) = transTypeSig tyy

          ctorStart x = isUpper x || x `elem` ":("
          kind | ctorStart $ head nam = DataCtorItem
               | otherwise = FunctionItem
transDecl x (HSE.TypeSig o names tyy) = fmap f $ sequence [transDecl x $ HSE.TypeSig o [name] tyy | name <- names]
    where f xs = (concatMap fst xs, concatMap snd xs)

transDecl x (ClassDecl s ctxt hd _ _) = Just $ fact (kinds True $ transDeclHead ctxt hd) $ textItem
    {itemName=nam, itemKey=nam, itemKind=ClassItem
    ,itemURL="#t:" ++ esc nam
    ,itemDisp=x `formatTags` [(cols $ head $ srcInfoPoints s, TagEmph),(cols snam,TagBold)]}
    where (snam,nam) = findName hd

transDecl x (TypeDecl s hd ty) = Just $ fact (FactAlias from to:kinds False from++kinds False to) $ textItem
    {itemName=nam, itemKey=nam, itemKind=TypeSynonymItem
    ,itemURL="#t:" ++ esc nam
    ,itemDisp=x `formatTags` [(cols $ head $ srcInfoPoints s, TagEmph),(cols snam,TagBold)]}
    where (snam,nam) = findName hd
          from = transDeclHead Nothing hd
          to = transTypeSig ty

transDecl x (DataDecl _ dat ctxt hd _ _) = Just $ fact (kinds False $ transDeclHead ctxt hd) $ textItem
    {itemName=nam, itemKey=nam, itemKind=TypeCtorItem
    ,itemURL="#t:" ++ esc nam
    ,itemDisp=x `formatTags` [(cols $ srcInfoSpan $ ann dat, TagEmph),(cols snam,TagBold)]}
    where (snam,nam) = findName hd

transDecl x (InstDecl _ _ hd _) = Just (FactInstance t:kinds True t, [])
    where t = transInstRule hd

transDecl _ _ = Nothing


esc = concatMap f
    where
        f x | isAlphaNum x = [x]
            | otherwise = "-" ++ show (ord x) ++ "-"


typeArgsPos :: HSE.Type S -> [SrcSpan]
typeArgsPos (TyForall _ _ _ x) = typeArgsPos x
typeArgsPos (TyFun _ x y) = srcInfoSpan (ann x) : typeArgsPos y
typeArgsPos (TyParen _ x) = typeArgsPos x
typeArgsPos x = [srcInfoSpan $ ann x]




cols :: SrcSpan -> (Int,Int)
cols x = (srcSpanStartColumn x - 1, srcSpanEndColumn x - 1)

findName :: Data a => a -> (SrcSpan,String)
findName x = case universeBi x of
        Ident s x : _ -> (srcInfoSpan s,x)
        Symbol s x : _ -> (srcInfoSpan s,x)

unbracket ('(':xs) | ")" `isSuffixOf` xs && nub ys `notElem` ["",","] = ys
    where ys = init xs
unbracket x = x


transType :: HSE.Type S -> Type
transType (TyForall _ _ _ x) = transType x
transType (TyFun _ x y) = TFun $ transType x : fromTFun (transType y)
transType (TyTuple _ x xs) = tApp (TLit $ "(" ++ h ++ replicate (length xs - 1) ',' ++ h ++ ")") $ map transType xs
    where h = ['#' | x == Unboxed]
transType (TyList _ x) = TApp (TLit "[]") [transType x]
transType (TyApp _ x y) = tApp a (b ++ [transType y])
    where (a,b) = fromTApp $ transType x
transType (TyVar _ x) = TVar $ prettyPrint x
transType (TyCon _ x) = TLit $ unbracket $ prettyPrint x
transType (TyParen _ x) = transType x
transType (TyInfix _ y1 x y2) = TApp (TLit $ unbracket $ prettyPrint x) [transType y1, transType y2]
transType (TyKind _ x _) = transType x
transType (TyPromoted _ _) = TLit "promoted"
transType (TyParArray _ x) = TApp (TLit "[::]") [transType x]
transType (TyEquals _ x y) = TApp (TLit "~") [transType x, transType y]
transType (TySplice _ _) = TLit "splice"
transType (TyBang _ _ x) = transType x

transContext :: Maybe (Context S) -> Constraint
transContext = maybe [] g
    where
        g (CxSingle _ x) = f x
        g (CxTuple _ xs) = concatMap f xs
        g _ = []

        f (ClassA _ x ys) = [TApp (TLit $ unbracket $ prettyPrint x) $ map transType ys]
        f (InfixA s y1 x y2) = f $ ClassA s x [y1,y2]
        f _ = []


transTypeSig :: HSE.Type S -> TypeSig
transTypeSig (TyParen _ x) = transTypeSig x
transTypeSig (TyForall _ _ con ty) = TypeSig (transContext con) $ transType ty
transTypeSig x = TypeSig [] $ transType x


transDeclHead :: Maybe (Context S) -> DeclHead S -> TypeSig
transDeclHead x y = TypeSig (transContext x) $ f y
    where f (DHead _ name) = TLit $ unbracket $ prettyPrint name
          f (DHInfix s a b) = f $ DHApp s (DHead s b) a
          f (DHParen _ x) = f x
          f (DHApp _ a b) = ttApp (f a) [transVar b]

transInstRule :: InstRule S -> TypeSig
transInstRule (IParen _ x) = transInstRule x
transInstRule (IRule _ _ ctxt hd) = transInstHead ctxt hd

transInstHead :: Maybe (Context S) -> InstHead S -> TypeSig
transInstHead x y = TypeSig (transContext x) $ f y
    where f (IHCon _ name) = TLit $ unbracket $ prettyPrint name
          f (IHInfix s x y) = f $ IHApp s (IHCon s y) x
          f (IHParen _ x) = f x
          f (IHApp _ t x) = ttApp (f t) [transType x]

transVar :: TyVarBind S -> Type
transVar (KindedVar _ nam _) = TVar $ prettyPrint nam
transVar (UnkindedVar _ nam) = TVar $ prettyPrint nam


---------------------------------------------------------------------

emph = TagEmph . Str
bold = TagBold . Str
space = Str " "


-- collect the kind facts, True for the outer fact is about a class
kinds :: Bool -> TypeSig -> [Fact]
kinds cls (TypeSig x y) = concatMap (f True) x ++ f cls y
    where
        f cls (TApp (TLit c) ys) = add cls c (length ys) ++
                                   if cls then [] else concatMap (f False) ys
        f cls (TLit c) = add cls c 0
        f cls x = if cls then [] else concatMap (f False) $ children x

        add cls c i = [(if cls then FactClassKind else FactDataKind) c i | not $ isTLitTuple c]
