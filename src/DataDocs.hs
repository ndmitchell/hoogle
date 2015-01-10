{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataDocs(allocIdentifiers, lookupIdentifier) where

import Language.Haskell.Exts.Annotated
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad

import Type


allocIdentifiers :: FilePath -> [Item] -> IO [(Maybe Id, Items)]
allocIdentifiers file xs = withBinaryFile (file <.> "docs") WriteMode $ \h -> do
    forM xs $ \x -> case x of
        Item{..} | Just s <- showItem itemItem -> do
            i <- Id . fromIntegral <$> hTell h
            hPutStrLn h $ show i ++ " " ++ s
            hPutStrLn h itemURL
            hPutStrLn h $ intercalate ", " $ for itemParents $ \xs -> unwords ["<a href=\"" ++ b ++ ">" ++ a ++ "</a>" | (a,b) <- xs]
            hPutStrLn h $ unlines $ replace [""] ["."] $ lines itemDocs
            return $ (Just i, itemItem)
        Item{..} -> return (Nothing, itemItem)
    -- write all the URLs, docs and enough info to pretty print it to a result
    -- and replace each with an identifier (index in the space) - big reduction in memory
    where
        showItem :: Items -> Maybe String
        showItem ITag{} = Nothing
        showItem (IDecl InstDecl{}) = Nothing
        showItem (IDecl x) = Just $ trimStart $ unwords $ words $ prettyPrint $ fmap (const noLoc) x
        showItem (IKeyword x) = Just $ "<b>keyword</b> " ++ x
        showItem (IPackage x) = Just $ "<b>package</b> " ++ x
        showItem (IModule x) = Just $ "<b>module</b> " ++ x


lookupIdentifier :: FilePath -> Int -> IO (URL, Documentation)
lookupIdentifier = undefined
