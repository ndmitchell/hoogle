{-# LANGUAGE ViewPatterns, TupleSections, ScopedTypeVariables, DeriveDataTypeable, ForeignFunctionInterface, GADTs #-}

module Output.Names(writeNames, searchNames) where

import Data.List.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Storable as V
import General.Str
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types
import Control.Exception
import System.IO.Unsafe
import Data.Maybe

import Input.Item
import General.Util
import General.Store

foreign import ccall text_search_bound :: CString -> IO CInt

foreign import ccall text_search :: CString -> Ptr CString -> CInt -> Ptr CInt -> IO CInt


data NamesSize a where NamesSize :: NamesSize Int deriving Typeable
data NamesItems a where NamesItems :: NamesItems (V.Vector TargetId) deriving Typeable
data NamesText a where NamesText :: NamesText BS.ByteString deriving Typeable

writeNames :: StoreWrite -> [(Maybe TargetId, Item)] -> IO ()
writeNames store xs = do
    let (ids, strs) = unzip [(i, [' ' | isUpper1 name] ++ lower name) | (Just i, x) <- xs, name <- itemNamePart x]
    let b = BS.intercalate (BS.pack "\0") (map bstrPack strs) `BS.append` BS.pack "\0\0"
    bound <- BS.unsafeUseAsCString b $ \ptr -> text_search_bound ptr
    storeWrite store NamesSize $ fromIntegral bound
    storeWrite store NamesItems $ V.fromList ids
    storeWrite store NamesText b

itemNamePart :: Item -> [String]
itemNamePart (IModule x) = [last $ splitOn "." x]
itemNamePart x = maybeToList $ itemName x

searchNames :: StoreRead -> Bool -> [String] -> [TargetId]
-- very important to not search for [" "] or [] since the output buffer is too small
searchNames store exact (filter (/= "") . map trim -> xs) = unsafePerformIO $ do
    let vs = storeRead store NamesItems
    -- if there are no questions, we will match everything, which exceeds the result buffer
    if null xs then return $ V.toList vs else do
        let tweak x = bstrPack $ [' ' | isUpper1 x] ++ lower x ++ "\0"
        bracket (mallocArray $ storeRead store NamesSize) free $ \result ->
            BS.unsafeUseAsCString (storeRead store NamesText) $ \haystack ->
                withs (map (BS.unsafeUseAsCString . tweak) xs) $ \needles ->
                    withArray0 nullPtr needles $ \needles -> do
                        found <- c_text_search haystack needles (if exact then 1 else 0) result
                        xs <- peekArray (fromIntegral found) result
                        return $ map ((vs V.!) . fromIntegral) xs

{-# NOINLINE c_text_search #-} -- for profiling
c_text_search a b c d = text_search a b c d
