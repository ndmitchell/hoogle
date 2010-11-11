
module Hoogle.TextBase.Parser(parseTextBase) where

import General.Code
import Hoogle.TextBase.Type
import Hoogle.TypeSig.All
import Language.Haskell.Exts hiding (TypeSig,Type)
import qualified Language.Haskell.Exts as HSE
import Hoogle.Util


parseTextBase :: String -> ([ParseError], TextBase)
parseTextBase = join . f [] "" . zip [1..] . lines
    where
        f com url [] = []
        f com url ((i,s):is)
            | "-- | " `isPrefixOf` s = f [drop 5 s] url is
            | "--" `isPrefixOf` s = f ([drop 5 s | com /= []] ++ com) url is
            | "@url " `isPrefixOf` s = f com (drop 5 s) is
            | all isSpace s = f [] "" is
            | otherwise = (case parseLine i s of
                               Left y -> Left y
                               Right (as,bs) -> Right (as,[b{itemURL=url, itemDocs=unlines $ reverse com} | b <- bs]))
                          : f [] "" is

        join xs = (err, (concat as, concat bs))
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
parseLine line x = case parseDeclWithMode defaultParseMode{extensions=[EmptyDataDecls,TypeOperators]} $ x ++ ex of
    ParseOk x -> maybe (Left $ ParseError line 1 "Can't translate") Right $ transDecl x
    ParseFailed pos msg -> case parseDeclWithMode defaultParseMode{extensions=[GADTs]} $ "data Data where " ++ x of
        ParseOk x | Just x <- transDecl x -> Right x
        _ -> Left $ ParseError line (srcLine pos) msg
    where ex = if "newtype " `isPrefixOf` x then " = Newtype" else ""


transDecl :: Decl -> Maybe ([Fact],[TextItem])
transDecl (ClassDecl _ ctxt name vars _ _) = Just $ itemClass $ transTypeCon ctxt (prettyPrint name) (map transVar vars)
transDecl (HSE.TypeSig _ [name] ty) = Just $ itemFunc (unbracket $ prettyPrint name) $ transTypeSig ty
transDecl (TypeDecl _ name vars ty) = Just $ itemAlias (transTypeCon [] (prettyPrint name) (map transVar vars)) (transTypeSig ty)
transDecl (DataDecl _ dat ctxt name vars _ _) = Just $ itemData (dat == DataType) $ transTypeCon ctxt (prettyPrint name) (map transVar vars)
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
