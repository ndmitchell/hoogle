{-# LANGUAGE PatternGuards #-}

module Hoogle.Language.Haskell.Input(parseInputHaskell) where

import General.Code
import Hoogle.Type.All
import Language.Haskell.Exts hiding (TypeSig,Type)
import qualified Language.Haskell.Exts as HSE
import Hoogle.Util
import Data.TagStr
import Data.Generics.Uniplate



parseInputHaskell :: String -> ([ParseError], Input)
parseInputHaskell = join . f [] "" . zip [1..] . lines
    where
        f com url [] = []
        f com url ((i,s):is)
            | "-- | " `isPrefixOf` s = f [drop 5 s] url is
            | "--" `isPrefixOf` s = f ([drop 5 s | com /= []] ++ com) url is
            | "@url " `isPrefixOf` s = f com (drop 5 s) is
            | all isSpace s = f [] "" is
            | otherwise = (case parseLine i s of
                               Left y -> Left y
                               Right (as,bs) -> Right (as,[b{itemURL=if null url then itemURL b else url, itemDocs=unlines $ reverse com} | b <- bs]))
                          : f [] "" is

        join xs = (err, (concat as, addModuleURLs $ concat bs))
            where (err,items) = unzipEithers xs
                  (as,bs) = unzip items


parseLine :: Int -> String -> Either ParseError ([Fact],[TextItem])
parseLine line ('@':str) = case a of
        "keyword" -> Right $ itemKeyword $ dropWhile isSpace b
        "package" -> Right $ itemPackage $ dropWhile isSpace b
        _ -> Left $ ParseError line 2 $ "Unknown attribute: " ++ a
    where (a,b) = break isSpace str
parseLine line x | a == "module" = Right $ itemModule $ split '.' $ dropWhile isSpace b
    where (a,b) = break isSpace x
parseLine line x = case parseDeclWithMode defaultParseMode{extensions=exts} $ x ++ ex of
    ParseOk x -> maybe (Left $ ParseError line 1 "Can't translate") Right $ transDecl x
    ParseFailed pos msg -> case parseDeclWithMode defaultParseMode{extensions=exts} $ "data Data where " ++ x of
        ParseOk x | Just x <- transDecl x -> Right x
        _ -> Left $ ParseError line (srcLine pos) msg
    where ex = if "newtype " `isPrefixOf` x then " = Newtype" else ""


exts = [EmptyDataDecls,TypeOperators,ExplicitForall,GADTs,KindSignatures,MultiParamTypeClasses
       ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams]

transDecl :: Decl -> Maybe ([Fact],[TextItem])
transDecl (ClassDecl _ ctxt name vars _ _) = Just $ itemClass $ transTypeCon ctxt (prettyPrint name) (map transVar vars)
transDecl (HSE.TypeSig _ [name] ty) = Just $ itemFunc (unbracket $ prettyPrint name) $ transTypeSig ty
transDecl (TypeDecl _ name vars ty) = Just $ itemAlias (transTypeCon [] (prettyPrint name) (map transVar vars)) (transTypeSig ty)
transDecl (DataDecl _ dat ctxt name vars _ _) = Just $ itemData (dat == DataType) $ transTypeCon ctxt (prettyPrint name) (map transVar vars)
transDecl (GDataDecl s dat ctxt name vars _ [] _) = transDecl $ DataDecl s dat ctxt name vars [] []
transDecl (InstDecl _ ctxt name vars _) = Just $ itemInstance $ transTypeCon ctxt (prettyPrint name) vars
transDecl (GDataDecl _ _ _ _ _ _ [GadtDecl _ name ty] _) = Just $ itemFunc (unbracket $ prettyPrint name) (transTypeSig ty)
transDecl _ = Nothing

unbracket ('(':xs) | ")" `isSuffixOf` xs = init xs
unbracket x = x


transType :: HSE.Type -> Type
transType (TyForall _ _ x) = transType x
transType (TyFun x y) = TFun $ transType x : fromTFun (transType y)
transType (TyTuple x xs) = tApp (TLit $ "(" ++ h ++ replicate (length xs - 1) ',' ++ h ++ ")") $ map transType xs
    where h = ['#' | x == Unboxed]
transType (TyList x) = TApp (TLit "[]") [transType x]
transType (TyApp x y) = tApp a (b ++ [transType y])
    where (a,b) = fromTApp $ transType x
transType (TyVar x) = TVar $ prettyPrint x
transType (TyCon x) = TLit $ unbracket $ prettyPrint x
transType (TyParen x) = transType x
transType (TyInfix y1 x y2) = TApp (TLit $ unbracket $ prettyPrint x) [transType y1, transType y2]
transType (TyKind x _) = transType x


transContext :: Context -> Constraint
transContext = concatMap f
    where
        f (ClassA x ys) = [TApp (TLit $ unbracket $ prettyPrint x) $ map transType ys]
        f (InfixA y1 x y2) = f $ ClassA x [y1,y2]
        f _ = []


transTypeSig :: HSE.Type -> TypeSig
transTypeSig (TyParen x) = transTypeSig x
transTypeSig (TyForall _ con ty) = TypeSig (transContext con) $ transType ty
transTypeSig x = TypeSig [] $ transType x


transTypeCon :: Context -> String -> [HSE.Type] -> TypeSig
transTypeCon x y z = TypeSig (transContext x) $ TApp (TLit $ unbracket y) $ map transType z


transVar :: TyVarBind -> HSE.Type
transVar (KindedVar nam _) = TyVar nam
transVar (UnkindedVar nam) = TyVar nam


addModuleURLs :: [TextItem] -> [TextItem]
addModuleURLs = f ""
    where
        f pkg (x:xs) | itemLevel x == 0 = x : f (head $ itemName x) xs
                     | itemLevel x == 1 = x{itemURL=url} : f pkg xs
            where url = "http://hackage.haskell.org/packages/archive/" ++ pkg ++ "/latest/doc/html/" ++ intercalate "-" (itemName x) ++ ".html"
        f pkg (x:xs) = x : f pkg xs
        f pkg [] = []


---------------------------------------------------------------------

textItem = TextItem 2 [] Nothing (Str "") "" ""

fact x y = (x,[y])

itemPackage x = fact [] $ textItem{itemLevel=0, itemName=[x],
    itemURL="http://hackage.haskell.org/package/" ++ x ++ "/",
    itemDisp=Tags [under "package",space,bold x]}

itemModule xs = fact [] $ textItem{itemLevel=1, itemName=xs,
    itemURL="", -- filled in by addModuleURLs
    itemDisp=Tags [under "module",Str $ " " ++ concatMap (++".") (init xs),bold $ last xs]}

itemKeyword x = fact [] $ textItem{itemName=[x],
    itemDisp=Tags [under "keyword",space,bold x]}

itemClass x = fact (kinds True x) $ textItem{itemName=[a],
    itemURL="#t:" ++ a,
    itemDisp=Tags $ [under "class",space,b]}
    where (a,b) = typeHead x

itemFunc nam typ@(TypeSig _ ty) = fact (ctr++kinds False typ) $ textItem{itemName=[nam],itemType=Just typ,
    itemURL="#v:" ++ nam,
    itemDisp=Tags[bold (operator nam), Str " :: ",renderTypeSig typ]}
    where operator xs@(x:_) | not $ isAlpha x || x `elem` "#_'" = "(" ++ xs ++ ")"
          operator xs = xs
          ctr = [FactCtorType nam y | isUpper $ head nam, TLit y <- [fst $ fromTApp $ last $ fromTFun ty]]

itemAlias from to = fact (FactAlias from to:kinds False from++kinds False to) $ textItem{itemName=[a],
    itemURL="#t:" ++ a,
    itemDisp=Tags[under "type",space,b]}
    where (a,b) = typeHead from

itemData d t = fact (kinds False t) $ textItem{itemName=[a],
    itemURL="#t:" ++ a,
    itemDisp=Tags[under (if d then "data" else "newtype"),space,b]}
    where (a,b) = typeHead t

itemInstance t = (FactInstance t:kinds True t, [])


under = TagUnderline . Str
bold = TagBold . Str
space = Str " "


typeHead :: TypeSig -> (String, TagStr)
typeHead (TypeSig con sig) = (a, Tags [Str $ showConstraint con, bold a, Str b])
    where (a,b) = break (== ' ') $ show sig


-- collect the kind facts, True for the outer fact is about a class
kinds :: Bool -> TypeSig -> [Fact]
kinds cls (TypeSig x y) = concatMap (f True) x ++ f cls y
    where
        f cls (TApp (TLit c) ys) = add cls c (length ys) ++
                                   if cls then [] else concatMap (f False) ys
        f cls (TLit c) = add cls c 0
        f cls x = if cls then [] else concatMap (f False) $ children x

        add cls c i = [(if cls then FactClassKind else FactDataKind) c i | not $ isTLitTuple c]
