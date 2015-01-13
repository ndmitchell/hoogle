{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Output.Types(writeTypes, searchTypes) where


import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import System.IO.Extra
import System.FilePath
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Generics.Uniplate.Data

import Type
import Util


{-
44K in total
main: fromList [(0,6260),(1,21265),(2,12188),(3,4557),(4,1681),(5,732),(6,363),(
7,220),(8,107),(9,80),(10,78),(11,35),(12,33),(13,17),(14,16),(15,16),(16,9),(17
,6),(18,7),(19,8),(20,5),(21,8),(22,2),(23,2),(24,3),(25,4),(26,2),(27,2),(28,3)
,(29,2),(30,2),(31,2),(32,1),(33,1),(34,1),(35,2),(36,1),(37,1),(38,2),(39,1),(4
0,2),(41,1),(42,1),(43,2),(44,1),(45,1),(46,1),(47,2),(48,1),(49,1),(50,1),(51,1
),(52,1),(53,1),(54,1),(55,1),(56,1),(57,1),(58,1),(59,1),(60,1),(61,1),(62,1),(
75,1)] -}

writeTypes :: Database -> [(Maybe Id, Items)] -> IO ()
writeTypes db@(Database file) xs = do
    rare <- writeRarity db $ map snd xs
    writeAlias db $ map snd xs
    writeInstance db $ map snd xs

    let ys = Map.toList $ Map.fromListWith (++) [(t, [i]) | (Just i, IDecl (TypeSig _ _ t)) <- xs]
    writeFileBinary (file <.> "types") $ unlines $ concat
        [ [unwords $ map show i, show $ preArity t, show $ preRarity rare t, unwords $ preNames t, pretty t]
        | (t,i) <- ys]


{-
--    error $ show $ Map.fromListWith (+) [(arity t, 1) | t <- Set.toList $ Set.fromList [t | (Just i, IDecl (TypeSig _ _ t)) <- xs]]

    let ys = Map.toAscList $ Map.fromListWith (++) [((arity t, t), [i]) | (Just i, IDecl (TypeSig _ _ t)) <- xs]
    writeFileBinary (file <.> "types") $ unlines
        [unwords $ map show i ++ [pretty t] | ((_,t),i) <- ys]
    writeFileBinary (file <.> "alias") $ unlines
        [pretty t | (_, IDecl t@TypeDecl{}) <- xs]
-}

searchTypes :: Database -> Type -> IO [Id]
searchTypes db@(Database file) q = do
    dbRare <- readRarity db
    dbAlias <- readAlias db
    dbInst <- readInstance db
    let chkArity = checkArity q
        chkRare = checkRarity dbRare q
        chkNames = checkNames dbAlias dbInst q
        match = matchType dbAlias dbInst q

    let f (ids:arity:rarity:names:typ:xs)
            | chkArity $ fst $ fromJust $ BS.readInt arity
            , chkRare $ fst $ fromJust $ BS.readInt rarity
            , chkNames $ map BS.unpack $ BS.words names
            , Just c <- match $ fromParseResult $ parseType $ BS.unpack typ
            = (c, map read $ words $ BS.unpack ids) : f xs
            | otherwise = f xs
        f _ = []
    concatMap snd . sortOn fst . f . BS.lines <$> BS.readFile (file <.> "types")


{-
NEED TO ADD:

.arities file which lists the extent of each group of arities
.approx file, which gives the approximate matching sets

and a full search mechanism

-}

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


---------------------------------------------------------------------
-- RARITY INFORMATION

data Rarity = Rarity Int (Map.Map String Int)

writeRarity :: Database -> [Items] -> IO Rarity
writeRarity  (Database file) xs = do
    let n = length xs
    let r = Map.fromListWith (+) $ concat [map (,1) $ Set.toList $ Set.fromList $ typeNames t | IDecl (TypeSig _ _ t) <- xs]
    writeFileBinary (file <.> "rare") $ unlines $
        show n :
        [x ++ " " ++ show i | (x,i) <- Map.toList r]
    return $ Rarity n r


readRarity :: Database -> IO Rarity
readRarity (Database file) = do
    count:rares <- fmap lines $ readFile' $ file <.> "rare"
    return $ Rarity (read count) $ Map.fromList $ map (second read . word1) rares


askRarity :: Rarity -> Type -> Int
askRarity (Rarity count mp) t = minimum $ count : map (\x -> Map.findWithDefault count x mp) (typeNames t)


---------------------------------------------------------------------
-- ALIAS INFORMATION

-- about 10% of aliases are duplicates
newtype Aliases = Aliases (Map.Map String [([String], Type)])

unpackAlias :: Decl -> Maybe (String, ([String], Type))
unpackAlias (TypeDecl _ name bind rhs) = Just (fromName name, (map (fromName . fromTyVarBind) bind, rhs))
unpackAlias _ = Nothing

packAlias :: String -> ([String], Type) -> Decl
packAlias name (bind, rhs) = TypeDecl noLoc (Ident name) (map (UnkindedVar . Ident) bind) rhs

writeAlias :: Database -> [Items] -> IO Aliases
writeAlias (Database file) xs = do
    let a = Map.fromListWith (++) [(a, [b]) | IDecl t <- xs, Just (a,b) <- [unpackAlias t]]
    writeFileBinary (file <.> "alias") $ unlines
        [pretty $ packAlias name body | (name, xs) <- Map.toList a, body <- xs]
    return $ Aliases a

readAlias :: Database -> IO Aliases
readAlias (Database file) = do
    src <- readFile $ file <.> "alias"
    return $ Aliases $ Map.fromListWith (++) [second return $ fromJust $ unpackAlias $ fromParseResult $ parseDecl x | x <- lines src]


aliasWords :: Aliases -> Type -> [String]
aliasWords (Aliases mp) t = g Set.empty $ f t
    where
        f t = [x | x@(c:cs) <- map fromName $ universeBi t, not $ isLower c]

        g seen (t:odo) | t `Set.member` seen = g seen odo
        g seen (t:odo) = let ys = Map.findWithDefault [] t mp in g (Set.insert t seen) (concatMap f ys ++ odo)
        g seen [] = Set.toList seen


---------------------------------------------------------------------
-- INSTANCE INFORMATION

newtype Instances = Instances [Decl]

writeInstance :: Database -> [Items] -> IO ()
writeInstance (Database file) xs =
    writeFileBinary (file <.> "instance") $ unlines
        [pretty t | IDecl t@InstDecl{} <- xs]

readInstance :: Database -> IO Instances
readInstance (Database file) = do
    src <- readFile $ file <.> "instance"
    return $ Instances $ map (fromParseResult . parseDecl) $ lines src


---------------------------------------------------------------------
-- PRECOMPUTE ARITY

preArity :: Type -> Int
preArity (TyForall _ _ x) = preArity x
preArity (TyFun _ x) = 1 + preArity x
preArity x = 0

checkArity :: Type -> (Int -> Bool)
checkArity (preArity -> q) = \a -> if q == 0 || a == 0 then q == a else abs (q - a) <= 1


---------------------------------------------------------------------
-- PRECOMPUTE RARITY

preRarity :: Rarity -> Type -> Int
preRarity = askRarity

checkRarity :: Rarity -> Type -> (Int -> Bool)
checkRarity r (askRarity r -> q) = \a -> q <= a * 5


---------------------------------------------------------------------
-- PRECOMPUTE RARITY

preNames :: Type -> [String]
preNames = typeNames

checkNames :: Aliases -> Instances -> Type -> ([String] -> Bool)
checkNames alias inst (typeNames -> q) = \a -> all (`elem` a) q


---------------------------------------------------------------------
-- REWRITE ENGINE

matchType :: Aliases -> Instances -> Type -> (Type -> Maybe Double)
matchType _ _ _ _ = Just 1

{-
data Term = Con String
          | Var String
          | App Term Term
-}

-- Either Int a ==> App (App (Con Either) (Con Int)) (Var a)


-- instance Eq Int
-- * C Eq a ==> a, missing context
-- * C Eq Int ==> Int, instantiate
-- * Int ==> C Eq a, abstract

-- rewrite :: [((Term,Term),Cost)] -> ([Term],Term) -> ([Term],Term) -> Maybe Cost
