{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, ForeignFunctionInterface #-}

module Output.Names(writeNames, searchNames) where

import Data.List.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Storable as V
import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Control.Exception

import Input.Type
import General.Util
import General.Store

foreign import ccall text_search_bound :: CString -> IO CInt

foreign import ccall text_search :: CString -> Ptr CString -> CInt -> Ptr CInt -> IO CInt


data Names = Names deriving Typeable


writeNames :: StoreOut -> [(Maybe Id, Item)] -> IO ()
writeNames store xs = do
    let (ids, strs) = unzip [(i, [' ' | isUName name] ++ lower name) | (Just i, x) <- xs, name <- toName x]
    let b = BS.intercalate (BS.pack "\0") (map BS.pack strs) `BS.append` BS.pack "\0\0"
    bound <- BS.unsafeUseAsCString b $ \ptr -> text_search_bound ptr
    writeStoreType store Names $ do
        writeStoreBS store $ intToBS $ fromIntegral bound
        writeStoreV store $ V.fromList ids
        writeStoreBS store b

toName :: Item -> [String]
toName (IKeyword x) = [x]
toName (IPackage x) = [x]
toName (IModule x) = [last $ splitOn "." x]
toName (IDecl x) = declNames x

searchNames :: StoreIn -> Bool -> [String] -> IO [(Score, Id)]
searchNames store exact (filter (/= "") -> xs) = do
    let [n,v,bs] = readStoreList $ readStoreType Names store
    -- if there are no questions, we will match everything, which exceeds the result buffer
    if null xs then return $ map (0,) $ V.toList $ readStoreV v else do
        let tweak x = BS.pack $ [' ' | isUName x] ++ lower x ++ "\0"
        bracket (mallocArray $ intFromBS $ readStoreBS n) free $ \result ->
            BS.unsafeUseAsCString (readStoreBS bs) $ \haystack ->
                withs (map (BS.unsafeUseAsCString . tweak) xs) $ \needles ->
                    withArray0 nullPtr needles $ \needles -> do
                        found <- c_text_search haystack needles (if exact then 1 else 0) result
                        xs <- peekArray (fromIntegral found) result
                        return [(0, vv V.! fromIntegral i) | let vv = readStoreV v, i <- xs]

{-# NOINLINE c_text_search #-} -- for profiling
c_text_search a b c d = text_search a b c d
