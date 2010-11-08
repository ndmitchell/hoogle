
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
                               Right y -> Right [(unlines $ reverse com, url, y)])
                          : f [] "" is

        join xs = (err, concat items)
            where (err,items) = unzipEithers xs


parseLine :: Int -> String -> Either ParseError TextItem
parseLine line ('@':str) = Right $ ItemAttribute a $ dropWhile isSpace b
    where (a,b) = break isSpace str
parseLine line x | a == "module" = Right $ ItemModule $ split '.' $ dropWhile isSpace b
    where (a,b) = break isSpace x
parseLine line x = case parseDeclWithMode defaultParseMode{extensions=[EmptyDataDecls,TypeOperators]} $ x ++ ex of
    ParseOk x -> maybe (Left $ ParseError line 1 "Can't translate") Right $ transDecl x
    ParseFailed pos msg -> Left $ ParseError line (srcLine pos) msg
    where ex = if "newtype " `isPrefixOf` x then " = Newtype" else ""


transDecl :: Decl -> Maybe TextItem
transDecl (ClassDecl _ ctxt name vars _ _) = Just $ ItemClass $ transTypeCon ctxt (prettyPrint name) (map transVar vars)
transDecl (HSE.TypeSig _ [name] ty) = Just $ ItemFunc (unbracket $ prettyPrint name) $ transTypeSig ty
transDecl (TypeDecl _ name vars ty) = Just $ ItemAlias (transTypeCon [] (prettyPrint name) (map transVar vars)) (transTypeSig ty)
transDecl (DataDecl _ dat ctxt name vars _ _) = Just $ ItemData kw $ transTypeCon ctxt (prettyPrint name) (map transVar vars)
    where kw = if dat == DataType then DataKeyword else NewTypeKeyword
transDecl (InstDecl _ ctxt name vars _) = Just $ ItemInstance $ transTypeCon ctxt (prettyPrint name) vars
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
