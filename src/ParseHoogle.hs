{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module ParseHoogle(parseHoogle) where

import Language.Haskell.Exts.Annotated
import Data.Char
import Data.List.Extra
import Type

hackage = "https://hackage.haskell.org/"


parseHoogle :: String -> [Either String (Tagged ItemEx)]
parseHoogle = f [] . lines
    where
        f com ((stripPrefix "-- " -> Just x):xs) = f (com ++ [x]) xs
        f com (x:xs) | all isSpace x = f [] xs
        f com (('@': (word1 -> (key,val))):xs) = Right (Tagged key val) : f [] xs
        f com ((stripPrefix "module " -> Just x):xs) = Right (Tagged "module" x) : f [] xs
        f com (x:xs) | ParseOk res <- parseDecl x = Right (Item $ ItemEx "http:" (unlines com) [] $ IDecl $ fmap (const ()) res) : f [] xs
        f com (x:xs) = Left ("Could not parse line: " ++ x) : f [] xs
        f com [] = []


{-
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
-}
