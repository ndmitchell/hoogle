{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}

module Output.Items(writeItems, lookupItem, listItems) where

import Language.Haskell.Exts
import Data.List.Extra
import Data.Maybe
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Codec.Compression.GZip as GZip

import Input.Item
import General.Util
import General.Store
import General.Conduit


outputItem :: (Id, (Target, Item)) -> [String]
outputItem (i, (Target{..}, item)) =
    [show i ++ " " ++ showItem item
    ,if null targetURL then "." else targetURL
    ,maybe "." (joinPair " ") targetPackage
    ,maybe "." (joinPair " ") targetModule
    ,targetItem] ++
    replace [""] ["."] (lines targetDocs)

inputItem :: [String] -> (Id, (Target, Item))
inputItem ((word1 -> (i,name)):url:pkg:modu:self:docs) =
    (read i
    ,(Target (if url == "." then "" else url) (f pkg) (f modu) self (unlines docs)
     ,fromMaybe (error $ "Failed to reparse: " ++ name) $ readItem name))
    where
        f "." = Nothing
        f x = Just (word1 x)

data Items = Items deriving Typeable

-- write all the URLs, docs and enough info to pretty print it to a result
-- and replace each with an identifier (index in the space) - big reduction in memory
writeItems :: StoreWrite -> (Conduit (Target, Item) IO (Maybe Id, Item) -> IO a) -> IO a
writeItems store act = do
    pos <- newIORef 0
    storeWriteType store Items $ storeWriteParts store $ act $
        awaitForever $ \(target, item) -> case item of
            IDecl InstDecl{} -> yield (Nothing, item)
            _ -> do
                i <- liftIO $ readIORef pos
                let bs = BS.concat $ LBS.toChunks $ GZip.compress $ UTF8.fromString $ unlines $ outputItem (Id i, (target, item))
                liftIO $ do
                    storeWriteBS store $ intToBS $ BS.length bs
                    storeWriteBS store bs
                    writeIORef pos $ i + fromIntegral (intSize + BS.length bs)
                yield (Just $ Id i, item)


listItems :: StoreRead -> [(Target, Item)]
listItems store = unfoldr f $ storeReadBS $ storeReadType Items store
    where
        f x | BS.null x = Nothing
            | (n,x) <- BS.splitAt intSize x
            , n <- intFromBS n
            , (this,x) <- BS.splitAt n x
            = Just (snd $ inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks [this], x)


lookupItem :: StoreRead -> (Id -> (Target, Item))
lookupItem store =
    let x = storeReadBS $ storeReadType Items store
    in \(Id i) ->
        let i2 = fromIntegral i
            n = intFromBS $ BS.take intSize $ BS.drop i2 x
        in snd $ inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks $ return $ BS.take n $ BS.drop (i2 + intSize) x
