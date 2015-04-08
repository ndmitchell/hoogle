{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}

module Output.Items(writeItems, lookupItem, listItems) where

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
import Codec.Compression.GZip as GZip

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
writeItems :: StoreWrite -> FilePath -> [Either String ItemEx] -> IO [(Maybe Id, Item)]
writeItems store file xs = do
    warns <- newIORef 0
    pos <- newIORef 0
    res <- storeWriteType store Items $ storeWriteParts store $ do
        withBinaryFile (file <.> "warn") WriteMode $ \herr -> do
            hSetEncoding herr utf8
            flip mapMaybeM xs $ \x -> case x of
                Right item@ItemEx{..} | f itemItem -> do
                    i <- readIORef pos
                    let bs = BS.concat $ LBS.toChunks $ GZip.compress $ UTF8.fromString $ unlines $ outputItem (Id i, item)
                    storeWriteBS store $ intToBS $ BS.length bs
                    storeWriteBS store bs
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

listItems :: StoreRead -> [ItemEx]
listItems store = unfoldr f $ storeReadBS $ storeReadType Items store
    where
        f x | BS.null x = Nothing
            | (n,x) <- BS.splitAt intSize x
            , n <- intFromBS n
            , (this,x) <- BS.splitAt n x
            = Just (snd $ inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks [this], x)


lookupItem :: StoreRead -> (Id -> ItemEx)
lookupItem store =
    let x = storeReadBS $ storeReadType Items store
    in \(Id i) ->
        let i2 = fromIntegral i
            n = intFromBS $ BS.take intSize $ BS.drop i2 x
        in snd $ inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks $ return $ BS.take n $ BS.drop (i2 + intSize) x
