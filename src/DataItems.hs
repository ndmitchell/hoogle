{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataItems(writeItems, lookupItem) where

import Language.Haskell.Exts
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad

import Type
import Util


writeItems :: FilePath -> [Item] -> IO [(Maybe Id, Items)]
writeItems file xs = withBinaryFile (file <.> "items") WriteMode $ \h -> do
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
        showItem (IDecl x) = Just $ pretty x
        showItem (IKeyword x) = Just $ "<b>keyword</b> " ++ x
        showItem (IPackage x) = Just $ "<b>package</b> " ++ x
        showItem (IModule x) = Just $ "<b>module</b> " ++ x


lookupItem :: Database -> Id -> IO [String]
lookupItem (Database file) (Id i) = withBinaryFile (file <.> "items") ReadMode $ \h -> do
    hSeek h AbsoluteSeek $ fromIntegral i
    xs <- replicateM 3 $ hGetLine h
    (xs ++) <$> f h
    where
        f h = do
            s <- hGetLine h
            if s == "" then return [] else (s:) <$> f h
