{-# LANGUAGE PatternGuards, DeriveDataTypeable, ScopedTypeVariables #-}

module General.Template(
    Template, templateFile, templateMarkup, templateApply, templateRender
    ) where

import Data.Data
import Data.Monoid
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import General.Str
import Control.Exception
import Data.Generics.Uniplate.Data
import Control.Applicative
import System.IO.Unsafe
import System.Directory
import Control.Monad
import Data.IORef
import Prelude

---------------------------------------------------------------------
-- TREE DATA TYPE

data Tree = Lam FilePath -- #{foo} defines a lambda
          | Var BStr -- a real variable
          | App Tree [(BStr, Tree)] -- applies a foo string to the lambda
          | Lit BStr
          | List [Tree]
            deriving (Typeable,Data,Show)


-- | Turn all Lam into Var/Lit
treeRemoveLam :: Tree -> IO Tree
treeRemoveLam = transformM f
    where
        f (Lam file) = List . parse <$> bstrReadFile file
        f x = return x

        parse x | Just (a,b) <- bstrSplitInfix (bstrPack "#{") x
                , Just (b,c) <- bstrSplitInfix (bstrPack "}") b
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

treeEval :: Tree -> [BStr]
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
        new <- forM files $ \file ->
            -- the standard getModificationTime message on Windows doesn't say the file
            getModificationTime file `catch` \(e :: IOException) ->
                fail $ "Failed: getModificationTime on " ++ file ++ ", " ++ show e
        if old == new then return t else do
            t <- treeOptimise <$> treeRemoveLam t0
            writeIORef ref (new,t)
            return t

templateTree :: Tree -> Template
templateTree t = Template t $ treeCache t

templateFile :: FilePath -> Template
templateFile = templateTree . Lam

templateMarkup :: Markup -> Template
templateMarkup = templateStr . renderMarkup

templateStr :: LBStr -> Template
templateStr = templateTree . List . map Lit . lbstrToChunks

templateApply :: Template -> [(String, Template)] -> Template
templateApply (Template t _) args = templateTree $ App t [(bstrPack a, b) | (a,Template b _) <- args]

templateRender :: Template -> [(String, Template)] -> IO LBStr
templateRender (Template _ t) args = do
    t <- t
    let Template t2 _ = templateApply (Template t $ return t) args
    lbstrFromChunks . treeEval <$> treeRemoveLam t2
