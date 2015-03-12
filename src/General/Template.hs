{-# LANGUAGE PatternGuards, ViewPatterns, DeriveDataTypeable #-}

module General.Template(
    Template, templateFile, templateStr, templateApply, templateRender
    ) where

import Data.Data
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Uniplate.Data
import Control.Applicative
import System.IO.Unsafe
import System.Directory
import Data.IORef
import Prelude

---------------------------------------------------------------------
-- TREE DATA TYPE

data Tree = Lam FilePath -- #{foo} defines a lambda
          | Var BS.ByteString -- a real variable
          | App Tree [(BS.ByteString, Tree)] -- applies a foo string to the lambda
          | Lit BS.ByteString
          | List [Tree]
            deriving (Typeable,Data,Show)


-- | Turn all Lam into Var/Lit
treeRemoveLam :: Tree -> IO Tree
treeRemoveLam = transformM f
    where
        f (Lam file) = List . parse <$> BS.readFile file
        f x = return x

        parse x | (a,b) <- BS.breakSubstring (BS.pack "#{") x
                , not $ BS.null b
                , (b,c) <- BS.break (== '}') $ BS.drop 2 b
                , not $ BS.null c
                = Lit a : Var b : parse (BS.drop 1 c)
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
        g xs = [Lit x | let x = BS.concat $ map fromLit a, not $ BS.null x] ++ g b
            where (a,b) = span isLit xs

treeEval :: Tree -> [BS.ByteString]
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

templateStr :: LBS.ByteString -> Template
templateStr = templateTree . List . map Lit . LBS.toChunks

templateApply :: Template -> [(String, Template)] -> Template
templateApply (Template t _) args = templateTree $ App t [(BS.pack a, b) | (a,Template b _) <- args]

templateRender :: Template -> [(String, Template)] -> IO LBS.ByteString
templateRender (Template _ t) args = do
    t <- t
    let Template t2 _ = templateApply (Template t $ return t) args
    LBS.fromChunks . treeEval <$> treeRemoveLam t2
