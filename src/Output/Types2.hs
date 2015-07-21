{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Output.Types2(writeTypes, searchTypes) where

{-
Approach:
Each signature is stored, along with a fingerprint
A quick search finds the most promising 100 fingerprints
A slow search ranks the 100 items, excluding some
-}


import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Extra
import Data.Maybe
import Data.Word
import Data.Function
import Data.List.Extra
import Data.Tuple.Extra
import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Functor.Identity
import System.FilePath
import System.IO.Extra

import Input.Type
import General.Util
import General.Store

-- Must be a unique Name per name.
-- First 100 are variables, rest are constructors.
-- More popular type constructors have higher numbers.
newtype Nam = Nam Word16 deriving (Eq,Ord)

-- FIXME: Temporary only
instance Show Nam where show (Nam x) = show x
instance Read Nam where readsPrec i s = map (first Nam) $ readsPrec i s

isCon, isVar :: Nam -> Bool
isVar (Nam x) = x < 100
isCon = not . isVar


data Types = Types deriving Typeable

writeTypes :: StoreWrite -> Maybe FilePath -> [(Maybe Id, Item)] -> IO ()
writeTypes store debug xs = storeWriteType store Types $ do
    ctors <- writeCtors store xs
    writeAlias store ctors xs
    writeInstance store ctors xs
    writeSigs store ctors xs

searchTypes :: StoreRead -> Type -> [Id]
searchTypes store typ =
    [id | (r1, r2, id) <- sig
        , (r2 == Nam 0) == (Set.size t <= 2) -- either you both have two concrete names, or neither of you do
        , r1 `Set.member` t, r2 `Set.member` t -- the rarest things in the answer are also in the search
        , fromNam r1 >= mn `div` 2 ] -- the rarest thing in the search isn't that much more rare than the answer
    where
        fromNam (Nam x) = x
        nams = mapMaybe (`Map.lookup` ctors) (typeNames typ)
        mn = if null nams then 0 else minimum $ map fromNam nams
        t = Set.fromList $ Nam 0 : nams

        [readCtors -> Ctors ctors, _, _, readSigs -> Sigs sig]
            = storeReadList $ storeReadType Types store



---------------------------------------------------------------------
-- CONSTRUCTOR INFORMATION

-- FIXME: Include suggestion information here (arity of known types Maybe=1, constructor types Just=Maybe)
newtype Ctors = Ctors (Map.Map String Nam)

writeCtors :: StoreWrite -> [(id,Item)] -> IO Ctors
writeCtors store xs = do
    let r = Map.fromListWith (+) $ concat [map (,1::Int) $ Set.toList $ Set.fromList $ typeNames t | (_, IDecl (TypeSig _ _ t)) <- xs]
    let r2 = Map.fromList $ zip (map fst $ sortOn snd $ Map.toList r) $ map Nam [100 ..]
    storeWriteBS store $ BS.pack $ unlines
        [x ++ " " ++ show i | (x,i) <- Map.toList r2]
    return $ Ctors r2


readCtors :: StoreRead -> Ctors
readCtors store = Ctors $ Map.fromList $ map (second read . word1) rares
    where rares = lines $ BS.unpack $ storeReadBS store


---------------------------------------------------------------------
-- ALIAS INFORMATION

-- about 10% of aliases are duplicates
newtype Aliases = Aliases (Map.Map String [([String], Type)])

unpackAlias :: Decl -> Maybe (String, ([String], Type))
unpackAlias (TypeDecl _ name bind rhs) = Just (fromName name, (map (fromName . fromTyVarBind) bind, rhs))
unpackAlias _ = Nothing

packAlias :: String -> ([String], Type) -> Decl
packAlias name (bind, rhs) = TypeDecl noLoc (Ident name) (map (UnkindedVar . Ident) bind) rhs

writeAlias :: StoreWrite -> Ctors -> [(Maybe Id, Item)] -> IO Aliases
writeAlias store ctors xs = do
    let a = Map.fromListWith (++) [(a, [b]) | (_, IDecl t) <- xs, Just (a,b) <- [unpackAlias t]]
    storeWriteBS store $ BS.pack $ unlines
        [pretty $ packAlias name body | (name, xs) <- Map.toList a, body <- xs]
    return $ Aliases a

readAlias :: StoreRead -> Aliases
readAlias store = Aliases $ Map.fromListWith (++) [second return $ fromMaybe (error "fromJust: readAlias") $ unpackAlias $ fromParseResult $ parseDecl x | x <- lines src]
    where src = BS.unpack $ storeReadBS store


---------------------------------------------------------------------
-- INSTANCE INFORMATION

newtype Instances = Instances [Decl]

writeInstance :: StoreWrite -> Ctors -> [(Maybe Id, Item)] -> IO ()
writeInstance store ctors xs =
    storeWriteBS store $ BS.pack $ unlines
        [pretty t | (_, IDecl t@InstDecl{}) <- xs]

readInstance :: StoreRead -> Instances
readInstance store = Instances $ map (fromParseResult . parseDecl) $ lines src
    where src = BS.unpack $ storeReadBS store


---------------------------------------------------------------------
-- SIGNATURES

data Fingerprint = Fingerprint
    {fpRare1 :: {-# UNPACK #-} !Nam -- Most rare ctor, or 0 if no rare stuff
    ,fpRare2 :: {-# UNPACK #-} !Nam -- 2nd rare ctor
    ,fpRare3 :: {-# UNPACK #-} !Nam -- 3rd rare ctor
    ,fpArity :: {-# UNPACK #-} !Word8 -- Artiy, where 0 = CAF
    ,fpTerms :: {-# UNPACK #-} !Word8 -- Number of terms (where 255 = 255 and above)
    }


-- rarest name, second rarest name, signature
newtype Sigs = Sigs [(Nam, Nam, Id)] -- should store the Sig itself

writeSigs :: StoreWrite -> Ctors -> [(Maybe Id, Item)] -> IO ()
writeSigs store (Ctors ctors) xs =
    storeWriteBS store $ BS.pack $ unlines
        [show (r1, r2, i)
            | (Just i, IDecl (TypeSig _ _ t)) <- xs
            , let rs = sort $ map (ctors Map.!) $ nubOrd $ typeNames t
            , let r1:r2:_ = rs ++ [Nam 0, Nam 0]
            ]

readSigs :: StoreRead -> Sigs
readSigs store = Sigs $ map read $ lines $ BS.unpack $ storeReadBS store


---------------------------------------------------------------------
-- SIMPLIFIED TYPES

-- FIXME: Delete the Read/Show instances
data Sig n = Sig [Ctx n] [Ty n] deriving (Show,Read) -- list of -> types
data Ctx n = Ctx n n deriving (Show,Read) -- context, second will usually be a free variable
data Ty n = Ty n [Ty n] deriving (Show,Read) -- type application, vectorised, all symbols may occur at multiple kinds


toSig :: Type -> Sig String
toSig = tyForall
    where
        -- forall at the top is different
        tyForall (TyParen x) = tyForall x
        tyForall (TyForall _ c t) | Sig cs ts <- tyForall t = Sig (toCtx c ++ cs) ts
        tyForall x = Sig [] $ tyFun x

        tyFun (TyParen x) = tyFun x
        tyFun (TyFun a b) = ty a : tyFun b
        tyFun x = [ty x]

        ty (TyForall _ _ x) = Ty "\\/" [ty x]
        ty x@TyFun{} = Ty "->" $ tyFun x
        ty (TyTuple box ts) = Ty (fromQName $ Special $ TupleCon box $ length ts) (map ty ts)
        ty (TyList x) = Ty "[]" [ty x]
        ty (TyParArray x) = Ty "[::]" [ty x]
        ty (TyApp x y) | Ty a b <- ty x = Ty a (b ++ [ty y])
        ty (TyVar x) = Ty (fromName x) []
        ty (TyCon x) = Ty (fromQName x) []
        ty (TyInfix a b c) = ty $ TyCon b `TyApp` a `TyApp` c
        ty (TyKind x _) = ty x
        ty (TyBang _ x) = ty x
        ty _ = Ty "_" []


toCtx :: Context -> [Ctx String]
toCtx = mapMaybe f
    where
        f (ParenA x) = f x
        f (ClassA con [TyVar var]) = Just $ Ctx (fromQName con) (fromName var)
        f _ = Nothing


-- all contexts are prefixed with ~
typeNames :: Type -> [String]
typeNames = typ
    where
        typ (TyForall _ cs t) = concatMap ctx cs ++ typ t
        typ (TyVar _) = []
        typ (TyCon c) = qnm c
        typ (TyInfix a b c) = typ a ++ qnm b ++ typ c
        typ (TyTuple _ xs) = ("(" ++ replicate (length xs - 1) ',' ++ ")") : concatMap typ xs
        typ (TyList x) = "[]" : typ x
        typ x = concatMap typ $ children x

        ctx (ClassA x _) = map ("~"++) $ qnm x
        ctx (InfixA a b c) = ctx $ ClassA b [a,c]
        ctx (ParenA x) = ctx x
        ctx _ = []

        qnm (Qual _ x) = [fromName x]
        qnm (UnQual x) = [fromName x]
        qnm (Special UnitCon) = ["()"]
        qnm (Special ListCon) = ["[]"]
        qnm (Special (TupleCon _ i)) = ["(" ++ replicate i ',' ++ ")"]
        qnm _ = []
