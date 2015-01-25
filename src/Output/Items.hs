{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Output.Items(writeItems, lookupItem) where

import Language.Haskell.Exts
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad.Extra
import Control.DeepSeq
import Data.Maybe
import Data.IORef

import Input.Type
import General.Util


outputItem :: (Id, ItemEx) -> [String]
outputItem (i, ItemEx{..}) =
    [show i ++ " " ++ showItem itemItem
    ,if null itemURL then "." else itemURL
    ,maybe "." (joinPair " ") itemPackage
    ,maybe "." (joinPair " ") itemModule] ++
    replace [""] ["."] (lines itemDocs)

inputItem :: [String] -> (Id, ItemEx)
inputItem ((word1 -> (i,name)):url:pkg:modu:docs) = (,) (read i) $ ItemEx
    (fromMaybe (error $ "Failed to reparse: " ++ name) $ readItem name)
    (if url == "." then "" else url)
    (f pkg) (f modu) (unlines docs)
    where
        f "." = Nothing
        f x = Just (word1 x)


-- write all the URLs, docs and enough info to pretty print it to a result
-- and replace each with an identifier (index in the space) - big reduction in memory
writeItems :: FilePath -> [Either String ItemEx] -> IO [(Maybe Id, Item)]
writeItems file xs = do
    warns <- newIORef 0
    res <- withBinaryFile (file <.> "items") WriteMode $ \hout ->
        withBinaryFile (file <.> "warn") WriteMode $ \herr -> do
            hSetEncoding hout utf8
            hSetEncoding herr utf8
            flip mapMaybeM xs $ \x -> case x of
                _ | rnf (show x) `seq` False -> return Nothing -- avoid a space leak
                Right item | f $ itemItem item -> do
                    i <- Id . fromIntegral <$> hTell hout
                    hPutStrLn hout $ unlines $ outputItem (i, item)
                    return $ Just (Just i, itemItem item)
                Right ItemEx{..} -> return $ Just (Nothing, itemItem)
                Left err -> do modifyIORef warns (+1); hPutStrLn herr err; return Nothing
    warns <- readIORef warns
    unless (warns == 0) $
        putStrLn $ "Failed to parse " ++ show warns ++ " definitions, see " ++ file <.> "warn"
    return res
    where
        f :: Item -> Bool
        f (IDecl i@InstDecl{}) = False
        f x = True


lookupItem :: Database -> IO (Id -> IO ItemEx)
lookupItem (Database file) = do
    h <- openBinaryFile (file <.> "items") ReadMode
    hSetEncoding h utf8
    return $ \(Id i) -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        xs <- f h
        return $ snd $ inputItem xs
        where
            f h = do
                s <- hGetLine h
                if s == "" then return [] else (s:) <$> f h
