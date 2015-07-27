{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}

module Output.Items(writeItems, lookupItem, listItems) where

import Language.Haskell.Exts
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


outputItem :: (Id, Target) -> [String]
outputItem (i, Target{..}) =
    [show i
    ,if null targetURL then "." else targetURL
    ,maybe "." (joinPair " ") targetPackage
    ,maybe "." (joinPair " ") targetModule
    ,if null targetType then "." else targetType
    ,targetItem] ++
    replace [""] ["."] (lines targetDocs)

inputItem :: [String] -> (Id, Target)
inputItem (i:url:pkg:modu:typ:self:docs) =
    (read i
    ,Target (if url == "." then "" else url) (f pkg) (f modu) (if typ == "." then "" else typ) self (unlines docs))
    where
        f "." = Nothing
        f x = Just (word1 x)

data Items = Items deriving Typeable

-- write all the URLs, docs and enough info to pretty print it to a result
-- and replace each with an identifier (index in the space) - big reduction in memory
writeItems :: StoreWrite -> (Conduit (Target, Item) IO (Maybe Id, Item) -> IO a) -> IO a
writeItems store act = do
    storeWriteType store Items $ storeWriteParts store $ act $
        void $ (\f -> mapAccumMC f 0) $ \pos (target, item) -> case item of
            IDecl InstDecl{} -> return (pos, (Nothing, item))
            _ -> do
                let bs = BS.concat $ LBS.toChunks $ GZip.compress $ UTF8.fromString $ unlines $ outputItem (Id pos, target)
                liftIO $ do
                    storeWriteBS store $ intToBS $ BS.length bs
                    storeWriteBS store bs
                let pos2 = pos + fromIntegral (intSize + BS.length bs)
                return (pos2, (Just $ Id pos, item))


listItems :: StoreRead -> [Target]
listItems store = unfoldr f $ storeReadBS $ storeReadType Items store
    where
        f x | BS.null x = Nothing
            | (n,x) <- BS.splitAt intSize x
            , n <- intFromBS n
            , (this,x) <- BS.splitAt n x
            = Just (snd $ inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks [this], x)


lookupItem :: StoreRead -> (Id -> Target)
lookupItem store =
    let x = storeReadBS $ storeReadType Items store
    in \(Id i) ->
        let i2 = fromIntegral i
            n = intFromBS $ BS.take intSize $ BS.drop i2 x
        in snd $ inputItem $ lines $ UTF8.toString $ GZip.decompress $ LBS.fromChunks $ return $ BS.take n $ BS.drop (i2 + intSize) x
