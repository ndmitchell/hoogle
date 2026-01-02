{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-pattern-synonym-signatures #-}

module Input.ParseDecl (myParseDecl) where

import Prelude hiding (Foldable(..))
import Data.Char (isAlphaNum, isUpper)
import Data.Foldable (Foldable(..))
import Data.List.Extra (dropEnd1, drop1, enumerate, stripPrefix, unsnoc, isInfixOf)
import Data.List.NonEmpty qualified as NE (toList)
import Data.Maybe (isNothing)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Parser (parseDeclaration)
import GHC.Parser.Lexer (P (..), ParseResult (..), initParserState, mkParserOpts)
import GHC.Types.Fixity (Fixity (..), FixityDirection (..))
import GHC.Types.Name (nameOccName, tvName)
import GHC.Types.Name.Occurrence (OccName (..), occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc, rdrNameSpace)
import GHC.Types.SourceText (SourceText (..))
import GHC.Types.SrcLoc (GenLocated (..), mkRealSrcLoc, unLoc)
import GHC.Unit (GenModule (..))
import GHC.Utils.Error
import GHC.Utils.Outputable (Outputable (..), defaultSDocContext, runSDoc)
import Language.Haskell.Exts qualified as HSE
import Text.Read (readMaybe)

#if !MIN_VERSION_ghc_lib_parser(9,12,0)
pattern HsBang x y <- HsSrcBang _ x y
#endif

#if MIN_VERSION_ghc_lib_parser(9,12,0)
pattern MyFixity x y <- Fixity x y
#else
pattern MyFixity x y <- Fixity _ x y
#endif

#if MIN_VERSION_ghc_lib_parser(9,10,0)
pattern MyPrefixConGADT x <- PrefixConGADT _ x
pattern MyRecConGADT x <- RecConGADT _ x
#else
pattern MyPrefixConGADT x <- PrefixConGADT x
pattern MyRecConGADT x <- RecConGADT x _
#endif

#if !MIN_VERSION_ghc_lib_parser(9,14,0)
#define con_outer_bndrs con_bndrs
#endif

myParseDecl :: String -> HSE.ParseResult (HSE.Decl ())
myParseDecl str = case runGhcLibParser str of
    POk _state x -> case hsDeclToDecl (unLoc x) of
        Nothing -> HSE.ParseFailed HSE.noLoc str
        Just res -> HSE.ParseOk res
    PFailed _state -> HSE.ParseFailed HSE.noLoc str

hsDeclToDecl :: HsDecl GhcPs -> Maybe (HSE.Decl ())
hsDeclToDecl (TyClD _ (SynDecl{tcdLName, tcdTyVars = HsQTvs{hsq_explicit}, tcdRhs})) =
    Just $
        HSE.TypeDecl
            ()
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc tcdLName)
                hsq_explicit
            )
            (hsTypeToType $ unLoc tcdRhs)
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl{tcdLName, tcdTyVars = HsQTvs{hsq_explicit}, tcdDataDefn = HsDataDefn{dd_cons = DataTypeCons _ [], dd_ctxt, dd_kindSig = Nothing}})) =
    Just $
        HSE.DataDecl
            ()
            (HSE.DataType ())
            (fmap (hsTypesToContext . unLoc) dd_ctxt)
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc tcdLName)
                hsq_explicit
            )
            []
            []
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl{tcdLName, tcdTyVars = HsQTvs{hsq_explicit}, tcdDataDefn = HsDataDefn{dd_cons = DataTypeCons _ [], dd_kindSig = Just kind}})) =
    Just $
        HSE.GDataDecl
            ()
            (HSE.DataType ())
            Nothing
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc tcdLName)
                hsq_explicit
            )
            (Just $ hsTypeToType $ unLoc kind)
            []
            []
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl{tcdLName, tcdTyVars = HsQTvs{hsq_explicit}, tcdDataDefn = HsDataDefn{dd_cons = DataTypeCons _ [L _ (ConDeclGADT{con_names, con_outer_bndrs, con_g_args = MyPrefixConGADT args, con_res_ty, con_mb_cxt})]}})) =
    Just $
        HSE.GDataDecl
            ()
            (HSE.DataType ())
            Nothing
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc tcdLName)
                hsq_explicit
            )
            Nothing
            ( map
                ( \con_name ->
                    HSE.GadtDecl
                        ()
                        (rdrNameToName $ unLoc con_name)
                        Nothing
                        Nothing
                        Nothing
                        ( maybe id (\bs -> applyTyForall (Just bs) Nothing) (hsOuterTyVarBndrsToFoo $ unLoc con_outer_bndrs) $
                            maybe id (applyTyForall Nothing . Just . hsTypesToContext . unLoc) con_mb_cxt $
                                foldr
#if MIN_VERSION_ghc_lib_parser(9,14,0)
                                    (\CDF{cdf_type} -> HSE.TyFun () (hsTypeToType $ unLoc cdf_type))
#else
                                    (\(HsScaled _ a) -> HSE.TyFun () (hsTypeToType $ unLoc a))
#endif
                                    (hsTypeToType $ unLoc con_res_ty)
                                    args
                        )
                )
                (NE.toList con_names)
            )
            []
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl{tcdLName, tcdTyVars = HsQTvs{hsq_explicit}, tcdDataDefn = HsDataDefn{dd_cons = DataTypeCons _ [L _ (ConDeclGADT{con_names, con_outer_bndrs, con_g_args = MyRecConGADT (L _ args), con_res_ty, con_mb_cxt})]}})) =
    Just $
        HSE.GDataDecl
            ()
            (HSE.DataType ())
            Nothing
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc tcdLName)
                hsq_explicit
            )
            Nothing
            ( map
                ( \con_name ->
                    HSE.GadtDecl
                        ()
                        (rdrNameToName $ unLoc con_name)
                        Nothing
                        Nothing
                        (Just $ map (conDeclFieldToFieldDecl . unLoc) args)
                        ( maybe id (HSE.TyForall () Nothing . Just . hsTypesToContext . unLoc) con_mb_cxt $
                            maybe id (\bs -> HSE.TyForall () (Just bs) Nothing) (hsOuterTyVarBndrsToFoo $ unLoc con_outer_bndrs) $
                                hsTypeToType $
                                    unLoc con_res_ty
                        )
                )
                (NE.toList con_names)
            )
            []
hsDeclToDecl (TyClD _ (FamDecl{tcdFam = FamilyDecl{fdLName, fdInfo = DataFamily, fdTyVars = HsQTvs{hsq_explicit}, fdResultSig}})) =
    Just $
        HSE.DataFamDecl
            ()
            Nothing
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc fdLName)
                hsq_explicit
            )
            (familyResultSigToResultSig $ unLoc fdResultSig)
hsDeclToDecl (TyClD _ (FamDecl{tcdFam = FamilyDecl{fdLName, fdTyVars = HsQTvs{hsq_explicit}, fdResultSig, fdInjectivityAnn}})) =
    Just $
        HSE.TypeFamDecl
            ()
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc fdLName)
                hsq_explicit
            )
            (familyResultSigToResultSig $ unLoc fdResultSig)
            (fmap (injectivityAnnToInjectivityInfo . unLoc) fdInjectivityAnn)
hsDeclToDecl (TyClD _ (GHC.Hs.ClassDecl{tcdCtxt, tcdLName, tcdTyVars = HsQTvs{hsq_explicit}, tcdFDs})) =
    Just $
        HSE.ClassDecl
            ()
            (fmap (hsTypesToContext . unLoc) tcdCtxt)
            ( foldl'
                (\acc (L _ tv) -> HSE.DHApp () acc (hsTyVarBndrToTyVarBind tv))
                (HSE.DHead () $ rdrNameToName $ unLoc tcdLName)
                hsq_explicit
            )
            (map (funDepToFunDep . unLoc) tcdFDs)
            Nothing
hsDeclToDecl (SigD _ (GHC.Hs.TypeSig _ names (HsWC{hswc_body = L _ HsSig{sig_body}}))) =
    Just $
        HSE.TypeSig
            ()
            (map (rdrNameToName . unLoc) names)
            (hsTypeToType $ unLoc sig_body)
hsDeclToDecl (SigD _ (GHC.Hs.PatSynSig _ names (L _ HsSig{sig_body}))) =
    Just $ case hsTypeToType (unLoc sig_body) of
        HSE.TyForall () Nothing (Just ctx1) (HSE.TyForall () Nothing (Just ctx2) ty) ->
            HSE.PatSynSig
                ()
                (map (rdrNameToName . unLoc) names)
                Nothing
                (Just ctx1)
                Nothing
                (Just ctx2)
                ty
        HSE.TyForall () Nothing (Just ctx) ty ->
            HSE.PatSynSig
                ()
                (map (rdrNameToName . unLoc) names)
                Nothing
                (Just ctx)
                Nothing
                Nothing
                ty
        ty ->
            HSE.PatSynSig
                ()
                (map (rdrNameToName . unLoc) names)
                Nothing
                Nothing
                Nothing
                Nothing
                ty
hsDeclToDecl (SigD _ (FixSig _ (FixitySig _ names (MyFixity priority direction)))) =
    Just $
        HSE.InfixDecl
            ()
            (fixityDirectionToAssoc direction)
            (Just priority)
            (map (varOpOrConOp . rdrNameToName . unLoc) names)
hsDeclToDecl (InstD _ (ClsInstD{cid_inst = ClsInstDecl{cid_poly_ty = (L _ HsSig{sig_bndrs, sig_body})}})) =
    Just $ case hsTypeToType (unLoc sig_body) of
        HSE.TyForall () Nothing ctxt body ->
            HSE.InstDecl
                ()
                Nothing
                (HSE.IRule () (hsOuterTyVarBndrsToFoo sig_bndrs) ctxt (typeToInstHead body))
                Nothing
        body ->
            HSE.InstDecl
                ()
                Nothing
                (HSE.IRule () (hsOuterTyVarBndrsToFoo sig_bndrs) Nothing (typeToInstHead body))
                Nothing
hsDeclToDecl _ = Nothing

injectivityAnnToInjectivityInfo :: InjectivityAnn GhcPs -> HSE.InjectivityInfo ()
injectivityAnnToInjectivityInfo = \case
    InjectivityAnn _ lhs rhs ->
        HSE.InjectivityInfo () (rdrNameToName $ unLoc lhs) (map (rdrNameToName . unLoc) rhs)

#if MIN_VERSION_ghc_lib_parser(9,14,0)
conDeclFieldToFieldDecl :: HsConDeclRecField GhcPs -> HSE.FieldDecl ()
conDeclFieldToFieldDecl = \case
    HsConDeclRecField{cdrf_names, cdrf_spec = CDF{cdf_type}} ->
        HSE.FieldDecl
            ()
            (map (fieldOccToName . unLoc) cdrf_names)
            (hsTypeToType $ unLoc cdf_type)
#else
conDeclFieldToFieldDecl :: ConDeclField GhcPs -> HSE.FieldDecl ()
conDeclFieldToFieldDecl = \case
    ConDeclField{cd_fld_names, cd_fld_type} ->
        HSE.FieldDecl
            ()
            (map (fieldOccToName . unLoc) cd_fld_names)
            (hsTypeToType $ unLoc cd_fld_type)
#endif

fieldOccToName :: FieldOcc GhcPs -> HSE.Name ()
fieldOccToName = \case
    FieldOcc{foLabel} -> rdrNameToName $ unLoc foLabel

varOpOrConOp :: HSE.Name () -> HSE.Op ()
varOpOrConOp name = case name of
    HSE.Symbol () (':' : _) -> HSE.ConOp () name
    HSE.Ident () (c : _)
        | isUpper c -> HSE.ConOp () name
        | otherwise -> HSE.VarOp () name
    _ -> HSE.VarOp () name

familyResultSigToResultSig :: FamilyResultSig GhcPs -> Maybe (HSE.ResultSig ())
familyResultSigToResultSig = \case
    NoSig{} -> Nothing
    GHC.Hs.KindSig _ kind ->
        Just $ HSE.KindSig () $ hsTypeToType $ unLoc kind
    GHC.Hs.TyVarSig _ tvb ->
        Just $ HSE.TyVarSig () $ hsTyVarBndrToTyVarBind $ unLoc tvb

hsOuterTyVarBndrsToFoo :: HsOuterSigTyVarBndrs GhcPs -> Maybe [HSE.TyVarBind ()]
hsOuterTyVarBndrsToFoo = \case
    HsOuterImplicit{} -> Nothing
    HsOuterExplicit{hso_bndrs} -> Just $ map (hsTyVarBndrToTyVarBind . unLoc) hso_bndrs

funDepToFunDep :: GHC.Hs.FunDep GhcPs -> HSE.FunDep ()
funDepToFunDep = \case
    GHC.Hs.FunDep _ lhs rhs ->
        HSE.FunDep () (map (rdrNameToName . unLoc) lhs) (map (rdrNameToName . unLoc) rhs)

fixityDirectionToAssoc :: FixityDirection -> HSE.Assoc ()
fixityDirectionToAssoc = \case
    InfixL -> HSE.AssocLeft ()
    InfixR -> HSE.AssocRight ()
    InfixN -> HSE.AssocNone ()

hsTyVarBndrToTyVarBind ::
    HsTyVarBndr a GhcPs ->
    HSE.TyVarBind ()
#if MIN_VERSION_ghc_lib_parser(9,12,0)
hsTyVarBndrToTyVarBind = \case
    HsTvb _ _ (HsBndrVar _ (L _ var)) HsBndrNoKind{} ->
        HSE.UnkindedVar () (rdrNameToName var)
    HsTvb _ _ (HsBndrVar _ (L _ var)) (HsBndrKind _ (L _ kind)) ->
        HSE.KindedVar () (rdrNameToName var) (hsTypeToType kind)
    HsTvb _ _ (HsBndrWildCard _) _ ->
        HSE.UnkindedVar () (HSE.Ident () "_")
#else
hsTyVarBndrToTyVarBind = \case
    UserTyVar _ _ (L _ var) ->
        HSE.UnkindedVar () (rdrNameToName var)
    KindedTyVar _ _ (L _ var) (L _ kind) ->
        HSE.KindedVar () (rdrNameToName var) (hsTypeToType kind)
#endif

occNameToName :: OccName -> HSE.Name ()
occNameToName occ = case occNameString occ of
    xs@(x : _)
        | not (isAlphaNum x) && x /= '(' && x /= '_' -> HSE.Symbol () xs
    xs -> HSE.Ident () xs

rdrNameToName :: RdrName -> HSE.Name ()
rdrNameToName = occNameToName . rdrNameOcc

rdrNameToQName :: RdrName -> HSE.QName ()
rdrNameToQName = \case
    Unqual occName ->
        HSE.UnQual () $ occNameToName occName
    GHC.Types.Name.Reader.Qual modName occName ->
        HSE.Qual
            ()
            (HSE.ModuleName () (moduleNameString modName))
            (occNameToName occName)
    Orig modul occName ->
        HSE.Qual
            ()
            (HSE.ModuleName () (moduleNameString $ moduleName modul))
            (occNameToName occName)
    Exact name ->
        HSE.UnQual () $ occNameToName (nameOccName name)

hsTypeToType :: HsType GhcPs -> HSE.Type ()
hsTypeToType = \case
    HsListTy _ x ->
        HSE.TyList () $ hsTypeToType $ unLoc x
    HsTyVar _ _ (L _ (Exact x))
        | show (runSDoc (ppr x) defaultSDocContext) == "[]" ->
            HSE.TyCon () $ HSE.Special () $ HSE.ListCon ()
        | show (runSDoc (ppr x) defaultSDocContext) == "->" ->
            HSE.TyCon () $ HSE.Special () $ HSE.FunCon ()
        | Just n <- stripPrefix "Tuple" (occNameString (nameOccName x))
        , Just n' <- readMaybe n ->
            HSE.TyCon () $ HSE.Special () $ HSE.TupleCon () HSE.Boxed n'
    HsTyVar _ IsPromoted (L _ x) ->
        HSE.TyPromoted () $ HSE.PromotedCon () True $ rdrNameToQName x
    HsTyVar _ NotPromoted (L _ x) ->
        case rdrNameSpace x of
            ns
                | ns == tvName ->
                    HSE.TyVar () $ rdrNameToName x
            _ -> HSE.TyCon () $ rdrNameToQName x
    HsAppTy _ x y ->
        HSE.TyApp () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc y)
    HsFunTy _ _ x y ->
        HSE.TyFun () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc y)
    HsTupleTy _ HsBoxedOrConstraintTuple [] ->
        HSE.TyCon () $ HSE.Special () $ HSE.UnitCon ()
    HsTupleTy _ HsUnboxedTuple [] ->
        HSE.TyCon () $ HSE.Special () $ HSE.UnboxedSingleCon ()
    HsTupleTy _ boxed xs ->
        HSE.TyTuple () (hsTupleSortToBoxed boxed) (map (hsTypeToType . unLoc) xs)
    HsStarTy _ _ ->
        HSE.TyStar ()
#if !MIN_VERSION_ghc_lib_parser(9,14,0)
    HsBangTy _ (HsBang unpackedness strictness) x ->
        applyTyBang
            (srcStrictnessToBangType strictness)
            (srcUnpackednessToUnpackedness unpackedness)
            (hsTypeToType $ unLoc x)
#endif
    HsParTy _ x -> case hsTypeToType (unLoc x) of
        x'@HSE.TyKind{} -> x'
        x' -> HSE.TyParen () x'
    HsQualTy{hst_ctxt, hst_body} ->
        applyTyForall Nothing (Just $ hsTypesToContext $ unLoc hst_ctxt) $
            hsTypeToType $
                unLoc hst_body
    HsForAllTy{hst_tele = HsForAllInvis{hsf_invis_bndrs}, hst_body} ->
        applyTyForall
            (Just $ map (hsTyVarBndrToTyVarBind . unLoc) hsf_invis_bndrs)
            Nothing
            (hsTypeToType $ unLoc hst_body)
    -- TODO FIXME when migrating to ghc-lib-parser completely:
    -- HSE does not support forall with visible binders
    HsForAllTy{hst_tele = HsForAllVis{hsf_vis_bndrs}, hst_body} ->
        applyTyForall
            (Just $ map (hsTyVarBndrToTyVarBind . unLoc) hsf_vis_bndrs)
            Nothing
            (hsTypeToType $ unLoc hst_body)
    HsExplicitListTy _ IsPromoted xs ->
        HSE.TyPromoted () $ HSE.PromotedList () True (map (hsTypeToType . unLoc) xs)
    HsExplicitListTy _ NotPromoted [x] ->
        HSE.TyList () $ hsTypeToType $ unLoc x
    HsExplicitListTy _ NotPromoted xs ->
        HSE.TyPromoted () $ HSE.PromotedList () False (map (hsTypeToType . unLoc) xs)

#if MIN_VERSION_ghc_lib_parser(9,12,0)
    HsExplicitTupleTy _ IsPromoted [] ->
        HSE.TyPromoted () $ HSE.PromotedCon () True $ HSE.Special () $ HSE.UnitCon ()
    HsExplicitTupleTy _ IsPromoted xs ->
        HSE.TyPromoted () $ HSE.PromotedTuple () $ map (hsTypeToType . unLoc) xs
    HsExplicitTupleTy _ NotPromoted xs ->
        HSE.TyTuple () HSE.Boxed (map (hsTypeToType . unLoc) xs)
#else
    HsExplicitTupleTy _ [] ->
        HSE.TyPromoted () $ HSE.PromotedCon () True $ HSE.Special () $ HSE.UnitCon ()
    HsExplicitTupleTy _ xs ->
        HSE.TyPromoted () $ HSE.PromotedTuple () $ map (hsTypeToType . unLoc) xs
#endif

    HsOpTy _ _ x (L _ (Unqual y)) z
        | occNameString y == "~" ->
            HSE.TyEquals () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc z)
    HsOpTy _ promotion x (L _ (Exact y)) z
        | occNameString (nameOccName y) == ":" ->
            HSE.TyInfix
                ()
                (hsTypeToType $ unLoc x)
                (promotionFlagToMaybePromotedName promotion $ HSE.Special () $ HSE.Cons ())
                (hsTypeToType $ unLoc z)
    HsOpTy _ promotion x y z ->
        HSE.TyInfix
            ()
            (hsTypeToType $ unLoc x)
            (promotionFlagToMaybePromotedName promotion $ rdrNameToQName $ unLoc y)
            (hsTypeToType $ unLoc z)
    HsKindSig _ lhs rhs ->
        HSE.TyKind () (hsTypeToType $ unLoc lhs) (hsTypeToType $ unLoc rhs)
    HsTyLit _ (HsNumTy (SourceText txt) val) ->
        HSE.TyPromoted () $ HSE.PromotedInteger () val (unpackFSOrId txt)
    HsTyLit _ (HsStrTy (SourceText txt) val) ->
        HSE.TyPromoted () $ HSE.PromotedString () (unpackFS val) (drop1 $ dropEnd1 $ unpackFSOrId txt)
    HsSumTy _ xs ->
        HSE.TyUnboxedSum () $ map (hsTypeToType . unLoc) xs
    HsWildCardTy _ ->
        HSE.TyWildCard () Nothing
    -- Everything else cannot be represented in HSE, so replacing with a wildcard
    _ -> HSE.TyWildCard () Nothing

promotionFlagToMaybePromotedName ::
    PromotionFlag -> HSE.QName () -> HSE.MaybePromotedName ()
promotionFlagToMaybePromotedName = \case
    NotPromoted -> HSE.UnpromotedName ()
    IsPromoted -> HSE.PromotedName ()

applyTyForall ::
    Maybe [HSE.TyVarBind ()] -> Maybe (HSE.Context ()) -> HSE.Type () -> HSE.Type ()
applyTyForall mArg1 mArg2 = \case
    HSE.TyForall () Nothing mArg2' ty
        | isNothing mArg2 -> HSE.TyForall () mArg1 mArg2' ty
    ty -> HSE.TyForall () mArg1 mArg2 ty

#if !MIN_VERSION_ghc_lib_parser(9,14,0)
applyTyBang ::
    HSE.BangType () -> HSE.Unpackedness () -> HSE.Type () -> HSE.Type ()
applyTyBang bang unpack = \case
    HSE.TyBang () (HSE.NoStrictAnnot ()) unpack' ty
        | unpack == HSE.NoUnpackPragma () -> HSE.TyBang () bang unpack' ty
    HSE.TyBang () bang' (HSE.NoUnpackPragma ()) ty
        | bang == HSE.NoStrictAnnot () -> HSE.TyBang () bang' unpack ty
    HSE.TyApp () x y -> HSE.TyApp () (applyTyBang bang unpack x) y
    ty -> HSE.TyBang () bang unpack ty

srcStrictnessToBangType :: SrcStrictness -> HSE.BangType ()
srcStrictnessToBangType = \case
    SrcLazy -> HSE.LazyTy ()
    SrcStrict -> HSE.BangedTy ()
    NoSrcStrict -> HSE.NoStrictAnnot ()

srcUnpackednessToUnpackedness :: SrcUnpackedness -> HSE.Unpackedness ()
srcUnpackednessToUnpackedness = \case
    SrcUnpack -> HSE.Unpack ()
    SrcNoUnpack -> HSE.NoUnpack ()
    NoSrcUnpack -> HSE.NoUnpackPragma ()
#endif

typeToInstHead :: HSE.Type () -> HSE.InstHead ()
typeToInstHead = \case
    HSE.TyApp () x y -> HSE.IHApp () (typeToInstHead x) y
    HSE.TyCon () x -> HSE.IHCon () x
    HSE.TyInfix () x (HSE.UnpromotedName () y) z -> HSE.IHApp () (HSE.IHInfix () x y) z
    -- The rest happens only in ghc-prim, which are likely some magical forms.
    -- Let's skip them.
    _ -> HSE.IHCon () $ HSE.Special () $ HSE.UnitCon ()

hsTypesToContext ::
    [GenLocated SrcSpanAnnA (HsType GhcPs)] ->
    HSE.Context ()
hsTypesToContext = \case
    [] -> HSE.CxEmpty ()
    [x] -> HSE.CxSingle () $ hsTypeToAsst $ unLoc x
    xs -> HSE.CxTuple () $ map (hsTypeToAsst . unLoc) xs

hsTypeToAsst :: HsType GhcPs -> HSE.Asst ()
hsTypeToAsst = \case
    HsIParamTy _ name t ->
        HSE.IParam () (hsIPNameToIPName $ unLoc name) (hsTypeToType $ unLoc t)
    HsParTy _ t ->
        HSE.ParenA () $ hsTypeToAsst $ unLoc t
    t -> case hsTypeToType t of
        HSE.TyParen () ty -> HSE.ParenA () $ HSE.TypeA () ty
        ty -> HSE.TypeA () ty

hsIPNameToIPName :: HsIPName -> HSE.IPName ()
hsIPNameToIPName = HSE.IPDup () . unpackFS . hsIPNameFS

hsTupleSortToBoxed :: HsTupleSort -> HSE.Boxed
hsTupleSortToBoxed = \case
    HsUnboxedTuple -> HSE.Unboxed
    HsBoxedOrConstraintTuple -> HSE.Boxed

runGhcLibParser ::
    String ->
    GHC.Parser.Lexer.ParseResult (GenLocated SrcSpanAnnA (HsDecl GhcPs))
runGhcLibParser str
    | Just (str', ';') <- unsnoc str =
        runGhcLibParser str'
runGhcLibParser str = case runGhcLibParserWithExtensions almostAllExtensions str of
    PFailed{}
        | '#' `elem` str -> runGhcLibParserWithExtensions noUnboxed str
        | '*' `elem` str -> runGhcLibParserWithExtensions noStarIsType str
        | "pattern" `isInfixOf` str -> runGhcLibParserWithExtensions noPatternSynonyms str
    res -> res

allExtensions :: EnumSet.EnumSet Extension
allExtensions = EnumSet.fromList enumerate

almostAllExtensions :: EnumSet.EnumSet Extension
almostAllExtensions =
    foldr
        EnumSet.delete
        allExtensions
        [ Arrows -- makes "proc" a keyword
        , RecursiveDo -- makes "mdo" and "rec" keywords
        , StaticPointers -- makes "static" a keyword
        , TransformListComp -- makes "by", "group" and "using" keywords
        ]

noUnboxed :: EnumSet.EnumSet Extension
noUnboxed =
    foldr
        EnumSet.delete
        almostAllExtensions
        [ UnboxedSums
        , UnboxedTuples
        ]

noPatternSynonyms :: EnumSet.EnumSet Extension
noPatternSynonyms = EnumSet.delete PatternSynonyms almostAllExtensions

noStarIsType :: EnumSet.EnumSet Extension
noStarIsType = EnumSet.delete StarIsType almostAllExtensions

runGhcLibParserWithExtensions ::
    EnumSet.EnumSet Extension ->
    String ->
    GHC.Parser.Lexer.ParseResult (GenLocated SrcSpanAnnA (HsDecl GhcPs))
runGhcLibParserWithExtensions extensions str = unP parseDeclaration parseState
  where
#if MIN_VERSION_ghc_lib_parser(9,14,0)
    opts = mkParserOpts extensions emptyDiagOpts False False False False
#else
    opts = mkParserOpts extensions emptyDiagOpts [] False False False False
#endif
    dummyLocation = mkRealSrcLoc mempty 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer dummyLocation

#if !MIN_VERSION_ghc_lib_parser(9,8,0)
emptyDiagOpts :: DiagOpts
emptyDiagOpts = DiagOpts mempty mempty False False Nothing defaultSDocContext
#endif

#if MIN_VERSION_ghc_lib_parser(9,8,0)
unpackFSOrId = unpackFS
#else
unpackFSOrId = id
#endif
