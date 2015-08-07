{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}

module Output.Items(writeItems, lookupItem, listItems) where

import Control.Monad
import Data.List.Extra
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Codec.Compression.GZip as GZip

import Input.Item
import General.Util
import General.Store
import General.Conduit


outputItem :: Target -> [String]
outputItem Target{..} =
    [if null targetURL then "." else targetURL
    ,maybe "." (joinPair " ") targetPackage
    ,maybe "." (joinPair " ") targetModule
    ,if null targetType then "." else targetType
    ,targetItem] ++
    replace [""] ["."] (lines targetDocs)

inputItem :: [String] -> Target
inputItem (url:pkg:modu:typ:self:docs) =
    Target (if url == "." then "" else url) (f pkg) (f modu) (if typ == "." then "" else typ) self (unlines docs)
    where
        f "." = Nothing
        f x = Just (word1 x)

data Items = Items deriving Typeable

-- write all the URLs, docs and enough info to pretty print it to a result
-- and replace each with an identifier (index in the space) - big reduction in memory
writeItems :: StoreWrite -> (Conduit (Maybe Target, Item) IO (Maybe TargetId, Item) -> IO a) -> IO a
writeItems store act = do
    storeWriteType store Items $ storeWriteParts store $ act $
        void $ (\f -> mapAccumMC f 0) $ \pos (target, item) -> case target of
            Nothing -> return (pos, (Nothing, item))
            Just target -> do
                let bs = BS.concat $ LBS.toChunks $ GZip.compress $ UTF8.fromString $ unlines $ outputItem target
                liftIO $ do
                    storeWriteBS store $ intToBS $ BS.length bs
                    storeWriteBS store bs
                let pos2 = pos + fromIntegral (intSize + BS.length bs)
                return (pos2, (Just $ TargetId pos, item))


listItems :: StoreRead -> [Target]
listItems store = unfoldr f $ storeReadBS $ storeReadType Items store
    where
        f x | BS.null x = Nothing
            | (n,x) <- BS.splitAt intSize x
            , n <- intFromBS n
            , (this,x) <- BS.splitAt n x
            = Just (inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks [this], x)


lookupItem :: StoreRead -> (TargetId -> Target)
lookupItem store =
    let x = storeReadBS $ storeReadType Items store
    in \(TargetId i) ->
        let i2 = fromIntegral i
            n = intFromBS $ BS.take intSize $ BS.drop i2 x
        in inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks $ return $ BS.take n $ BS.drop (i2 + intSize) x
