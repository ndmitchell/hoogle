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


-- write all the URLs, docs and enough info to pretty print it to a result
-- and replace each with an identifier (index in the space) - big reduction in memory
writeItems :: FilePath -> [Either String ItemEx] -> IO [(Maybe Id, Item)]
writeItems file xs = do
    warns <- newIORef 0
    res <- withBinaryFile (file <.> "items") WriteMode $ \hout ->
        withBinaryFile (file <.> "warn") WriteMode $ \herr -> do
            flip mapMaybeM xs $ \x -> case x of
                Right ItemEx{..} | Just s <- f itemItem -> do
                    i <- Id . fromIntegral <$> hTell hout
                    hPutStrLn hout $ show i ++ " " ++ s
                    hPutStrLn hout itemURL
                    hPutStrLn hout $ show itemParents
                    hPutStrLn hout $ unlines $ replace [""] ["."] $ lines itemDocs
                    return $ Just (Just i, itemItem)
                Right ItemEx{..} -> return $ Just (Nothing, itemItem)
                Left err -> do modifyIORef warns (+1); hPutStrLn herr err; return Nothing
    warns <- readIORef warns
    unless (warns == 0) $
        putStrLn $ "Failed to parse " ++ show warns ++ " definitions, see " ++ file <.> "warn"
    return res
    where
        f :: Item -> Maybe String
        f (IDecl i@InstDecl{}) = rnf (show i) `seq` Nothing
        f x = rnf (show x) `seq` Just (showItem x)


lookupItem :: Database -> IO (Id -> IO ItemEx)
lookupItem (Database file) = do
    h <- openBinaryFile (file <.> "items") ReadMode
    return $ \(Id i) -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        [name,url,parents] <- replicateM 3 $ hGetLine h
        docs <- f h
        return $ ItemEx url (unlines docs) (read parents) (fromJust $ readItem $ snd $ word1 name)
        where
            f h = do
                s <- hGetLine h
                if s == "" then return [] else (s:) <$> f h
