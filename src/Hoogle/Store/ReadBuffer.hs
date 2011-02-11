{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Hoogle.Store.ReadBuffer(
    Buffer, newBuffer,
    setPos, getPos,
    getStorable, getByteString,
    ) where

import General.Base
import General.System
import Foreign
import qualified Data.ByteString as BS


bufferSize = 100 :: Int

data Buffer = Buffer {handle :: Handle, fptr :: ForeignPtr ()}

newBuffer :: Handle -> IO Buffer
newBuffer handle = do
    ptr <- mallocForeignPtrBytes bufferSize
    return $ Buffer handle ptr


getPos :: Buffer -> IO Word32
getPos Buffer{..} = fmap fromIntegral $ hTell handle

setPos :: Buffer -> Word32 -> IO ()
setPos b@Buffer{..} pos = do
    hSeek handle AbsoluteSeek $ fromIntegral pos


getStorable :: forall a . Storable a => Buffer -> IO a
getStorable Buffer{..} = do
    let n = sizeOf (undefined :: a)
    when (n > bufferSize) $ error $ "Buffer size overflow in getStorable"
    withForeignPtr fptr $ \ptr -> do
        hGetBuf handle ptr $ sizeOf (undefined :: a)
        peek $ castPtr ptr


getByteString :: Buffer -> Int -> IO BString
getByteString Buffer{..} n = BS.hGet handle n
