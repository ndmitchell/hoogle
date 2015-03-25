{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, ForeignFunctionInterface #-}

module Output.Names(writeNames, searchNames) where

import Data.List.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Storable as V
import General.Str
import Data.Typeable
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types
import Control.Exception
import System.IO.Unsafe

import Input.Type
import General.Util
import General.Store

foreign import ccall text_search_bound :: CString -> IO CInt

foreign import ccall text_search :: CString -> Ptr CString -> CInt -> Ptr CInt -> IO CInt


data Names = Names deriving Typeable


writeNames :: StoreWrite -> [(Maybe Id, Item)] -> IO ()
writeNames store xs = do
    let (ids, strs) = unzip [(i, [' ' | isUpper1 name] ++ lower name) | (Just i, x) <- xs, name <- toName x]
    let b = BS.intercalate (BS.pack "\0") (map strPack strs) `BS.append` BS.pack "\0\0"
    bound <- BS.unsafeUseAsCString b $ \ptr -> text_search_bound ptr
    storeWriteType store Names $ do
        storeWriteBS store $ intToBS $ fromIntegral bound
        storeWriteV store $ V.fromList ids
        storeWriteBS store b

toName :: Item -> [String]
toName (IKeyword x) = [x]
toName (IPackage x) = [x]
toName (IModule x) = [last $ splitOn "." x]
toName (IDecl x) = declNames x

searchNames :: StoreRead -> Bool -> [String] -> [Id]
searchNames store exact (filter (/= "") -> xs) = unsafePerformIO $ do
    let [n,v,bs] = storeReadList $ storeReadType Names store
    -- if there are no questions, we will match everything, which exceeds the result buffer
    if null xs then return $ V.toList $ storeReadV v else do
        let tweak x = strPack $ [' ' | isUpper1 x] ++ lower x ++ "\0"
        bracket (mallocArray $ intFromBS $ storeReadBS n) free $ \result ->
            BS.unsafeUseAsCString (storeReadBS bs) $ \haystack ->
                withs (map (BS.unsafeUseAsCString . tweak) xs) $ \needles ->
                    withArray0 nullPtr needles $ \needles -> do
                        found <- c_text_search haystack needles (if exact then 1 else 0) result
                        xs <- peekArray (fromIntegral found) result
                        let vs = storeReadV v
                        return $ map ((vs V.!) . fromIntegral) xs

{-# NOINLINE c_text_search #-} -- for profiling
c_text_search a b c d = text_search a b c d
