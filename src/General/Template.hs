{-# LANGUAGE PatternGuards, ViewPatterns, DeriveDataTypeable #-}

module General.Template(
    Template, templateFile, templateStr, templateApply, templateRender
    ) where

import Data.Data
import Data.Monoid
import General.UTF8
import Data.Generics.Uniplate.Data
import Control.Applicative
import System.IO.Unsafe
import System.Directory
import Data.IORef
import Prelude

---------------------------------------------------------------------
-- TREE DATA TYPE

data Tree = Lam FilePath -- #{foo} defines a lambda
          | Var UTF8 -- a real variable
          | App Tree [(UTF8, Tree)] -- applies a foo string to the lambda
          | Lit UTF8
          | List [Tree]
            deriving (Typeable,Data,Show)


-- | Turn all Lam into Var/Lit
treeRemoveLam :: Tree -> IO Tree
treeRemoveLam = transformM f
    where
        f (Lam file) = List . parse <$> utf8ReadFile file
        f x = return x

        parse x | Just (a,b) <- utf8SplitInfix (utf8Pack "#{") x
                , Just (b,c) <- utf8SplitInfix (utf8Pack "}") b
                = Lit a : Var b : parse c
        parse x = [Lit x]

treeRemoveApp :: Tree -> Tree
treeRemoveApp = f []
    where
        f seen (App t xs) = f (xs ++ seen) t
        f seen (Var x) | Just t <- lookup x seen = f seen t
        f seen x = descend (f seen) x

treeOptimise :: Tree -> Tree
treeOptimise = transform f . treeRemoveApp
    where
        fromList (List xs) = xs; fromList x = [x]
        toList [x] = x; toList xs = List xs
        isLit (Lit x) = True; isLit _ = False
        fromLit (Lit x) = x

        f = toList . g . concatMap fromList . fromList

        g [] = []
        g (x:xs) | not $ isLit x = x : g xs
        g xs = [Lit x | let x = mconcat $ map fromLit a, x /= mempty] ++ g b
            where (a,b) = span isLit xs

treeEval :: Tree -> [UTF8]
treeEval = f . treeRemoveApp
    where f (Lit x) = [x]
          f (List xs) = concatMap f xs
          f _ = []


---------------------------------------------------------------------
-- TEMPLATE DATA TYPE

-- a tree, and a pre-optimised tree you can create
data Template = Template Tree (IO Tree)

{-# NOINLINE treeCache #-}
treeCache :: Tree -> IO Tree
treeCache t0 = unsafePerformIO $ do
    let files = [x | Lam x <- universe t0]
    ref <- newIORef ([], treeOptimise t0)
    return $ do
        (old,t) <- readIORef ref
        new <- mapM getModificationTime files
        if old == new then return t else do
            t <- treeOptimise <$> treeRemoveLam t0
            writeIORef ref (new,t)
            return t

templateTree :: Tree -> Template
templateTree t = Template t $ treeCache t

templateFile :: FilePath -> Template
templateFile = templateTree . Lam

templateStr :: LUTF8 -> Template
templateStr = templateTree . List . map Lit . lutf8ToChunks

templateApply :: Template -> [(String, Template)] -> Template
templateApply (Template t _) args = templateTree $ App t [(utf8Pack a, b) | (a,Template b _) <- args]

templateRender :: Template -> [(String, Template)] -> IO LUTF8
templateRender (Template _ t) args = do
    t <- t
    let Template t2 _ = templateApply (Template t $ return t) args
    lutf8FromChunks . treeEval <$> treeRemoveLam t2
