{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards, DeriveDataTypeable, GADTs #-}

module Output.Items(writeItems, lookupItem, listItems) where

import Control.Monad
import Data.List.Extra
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Codec.Compression.GZip as GZip
import General.Str

import Input.Item
import General.Util
import General.Store
import General.Conduit


data Items a where Items :: Items BS.ByteString deriving Typeable


outputItem :: Target -> [String]
outputItem Target{..} =
    [if null targetURL then "." else targetURL
    ,maybe "." (joinPair " ") targetPackage
    ,maybe "." (joinPair " ") targetModule
    ,if null targetType then "." else targetType
    ,targetItem] ++
    replace [""] ["."] (lines targetDocs)

inputItem :: [String] -> Target
inputItem (url:pkg:modu:typ:self:docs) = targetExpandURL $
    Target (if url == "." then "" else url) (f pkg) (f modu) (if typ == "." then "" else typ) self (unlines $ replace ["."] [""] docs)
    where
        f "." = Nothing
        f x = Just $ word1 x

-- write all the URLs, docs and enough info to pretty print it to a result
-- and replace each with an identifier (index in the space) - big reduction in memory
writeItems :: StoreWrite -> (ConduitM (Maybe Target, item) (Maybe TargetId, item) IO () -> IO a) -> IO a
writeItems store act = act $ do
    void $ (\f -> mapAccumMC f 0) $ \pos (target, item) -> case target of
        Nothing -> return (pos, (Nothing, item))
        Just target -> do
            let bs = LBS.toStrict $ GZip.compress $ lbstrPack $ unlines $ outputItem target
            liftIO $ do
                storeWritePart store Items $ intToBS $ BS.length bs
                storeWritePart store Items bs
            let pos2 = pos + fromIntegral (intSize + BS.length bs)
            return (pos2, (Just $ TargetId pos, item))


listItems :: StoreRead -> [Target]
listItems store = unfoldr f $ storeRead store Items
    where
        f x | BS.null x = Nothing
            | (n,x) <- BS.splitAt intSize x
            , n <- intFromBS n
            , (this,x) <- BS.splitAt n x
            = Just (inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks [this], x)


lookupItem :: StoreRead -> (TargetId -> Target)
lookupItem store =
    let x = storeRead store Items
    in \(TargetId i) ->
        let i2 = fromIntegral i
            n = intFromBS $ BS.take intSize $ BS.drop i2 x
        in inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks $ return $ BS.take n $ BS.drop (i2 + intSize) x
