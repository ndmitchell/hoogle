{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataTypes(writeTypes, searchTypes) where


import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import System.IO.Extra
import System.FilePath
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.List
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
    alias <- writeAlias db $ map snd xs
    writeInstance db $ map snd xs

    let ys = Map.toList $ Map.fromListWith (++) [(t, [i]) | (Just i, IDecl (TypeSig _ _ t)) <- xs]
    writeFileBinary (file <.> "types") $ unlines $ concat
        [[unwords $ map show i, unwords $ aliasWords alias t, pretty t] | (t,i) <- ys]

    print $ length ys
    print $ Set.size $ Set.fromList $ map (aliasWords alias . fst) ys


{-
--    error $ show $ Map.fromListWith (+) [(arity t, 1) | t <- Set.toList $ Set.fromList [t | (Just i, IDecl (TypeSig _ _ t)) <- xs]]

    let ys = Map.toAscList $ Map.fromListWith (++) [((arity t, t), [i]) | (Just i, IDecl (TypeSig _ _ t)) <- xs]
    writeFileBinary (file <.> "types") $ unlines
        [unwords $ map show i ++ [pretty t] | ((_,t),i) <- ys]
    writeFileBinary (file <.> "alias") $ unlines
        [pretty t | (_, IDecl t@TypeDecl{}) <- xs]
-}

arity (TyForall _ _ x) = arity x
arity (TyFun _ x) = 1 + arity x
arity x = 0

searchTypes :: Database -> Type -> IO [Id]
searchTypes = error "searchType"

{-
NEED TO ADD:

.arities file which lists the extent of each group of arities
.approx file, which gives the approximate matching sets

and a full search mechanism

-}

---------------------------------------------------------------------
-- ALIAS INFORMATION

-- about 10% of aliases are duplicates
newtype Aliases = Aliases (Map.Map String [([String], Type)])

writeAlias :: Database -> [Items] -> IO Aliases
writeAlias (Database file) xs = do
    let a = Map.fromListWith (++) [(fromName name, [(map (fromName . fromTyVarBind) bind, rhs)]) | IDecl (TypeDecl _ name bind rhs) <- xs]
    writeFileBinary (file <.> "alias") $ unlines
        [pretty $ TypeDecl noLoc (Ident name) (map (UnkindedVar . Ident) bind) rhs | (name, xs) <- Map.toList a, (bind,rhs) <- xs]
    return $ Aliases a

readAlias :: Database -> IO Aliases
readAlias = undefined


aliasWords :: Aliases -> Type -> [String]
aliasWords (Aliases mp) t = g Set.empty $ f t
    where
        f t = [x | x@(c:cs) <- map fromName $ universeBi t, not $ isLower c]

        g seen (t:odo) | t `Set.member` seen = g seen odo
        g seen (t:odo) = let ys = Map.findWithDefault [] t mp in g (Set.insert t seen) (concatMap f ys ++ odo)
        g seen [] = Set.toList seen


---------------------------------------------------------------------
-- INSTANCES

writeInstance :: Database -> [Items] -> IO ()
writeInstance (Database file) xs =
    writeFileBinary (file <.> "instance") $ unlines
        [pretty t | IDecl t@InstDecl{} <- xs]


---------------------------------------------------------------------
-- REWRITE ENGINE

data Term = Con String
          | Var String
          | App Term Term


-- Either Int a ==> App (App (Con Either) (Con Int)) (Var a)


-- instance Eq Int
-- * C Eq a ==> a, missing context
-- * C Eq Int ==> Int, instantiate
-- * Int ==> C Eq a, abstract

-- rewrite :: [((Term,Term),Cost)] -> ([Term],Term) -> ([Term],Term) -> Maybe Cost
