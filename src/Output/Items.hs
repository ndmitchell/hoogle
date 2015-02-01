{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}

module Output.Items(writeItems, lookupItem) where

import Language.Haskell.Exts
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad.Extra
import Data.Maybe
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Input.Type
import General.Util
import General.Store


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

data Items = Items deriving Typeable

-- write all the URLs, docs and enough info to pretty print it to a result
-- and replace each with an identifier (index in the space) - big reduction in memory
writeItems :: StoreOut -> FilePath -> [Either String ItemEx] -> IO [(Maybe Id, Item)]
writeItems store file xs = do
    warns <- newIORef 0
    pos <- newIORef 0
    res <- writeStoreType store Items $ writeStoreParts store $ do
        withBinaryFile (file <.> "warn") WriteMode $ \herr -> do
            hSetEncoding herr utf8
            flip mapMaybeM xs $ \x -> case x of
                Right item@ItemEx{..} | f itemItem -> do
                    i <- readIORef pos
                    let bs = BS.concat $ LBS.toChunks $ UTF8.fromString $ unlines $ outputItem (Id i, item)
                    writeStoreBS store $ intToBS $ BS.length bs
                    writeStoreBS store bs
                    writeIORef pos $ i + fromIntegral (intSize + BS.length bs)
                    return $ Just (Just $ Id i, itemItem)
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


lookupItem :: StoreIn -> IO (Id -> IO ItemEx)
lookupItem store = do
    let x = readStoreBS $ readStoreType Items store
    return $ \(Id i) -> do
        let i2 = fromIntegral i
        let n = intFromBS $ BS.take intSize $ BS.drop i2 x
        return $ snd $ inputItem $ lines $ UTF8.toString $ LBS.fromChunks $ return $ BS.take n $ BS.drop (i2 + intSize) x
