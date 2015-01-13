{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

-- I tried switching to blaze-builder, but this buffer is massively faster
module Hoogle.Store.WriteBuffer(
    Buffer, withBuffer,
    putStorable, putByteString,
    patch, getPos
    ) where

import General.Base
import General.System
import Data.IORef
import Foreign
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS


bufferSize = 10000 :: Word32


-- (number in file, number in buffer)
data Buffer = Buffer
    {handle :: Handle -- the handle we are writing to
    ,ptr :: Ptr () -- the current buffer
    ,inFile :: IORef Word32 -- the number of bytes on the disk
    ,inBuffer :: IORef Word32 -- the number of bytes in the buffer
    ,patchup :: IORef [Patchup]
    }

data Patchup = !Word32 := !Word32

writeRef ref v = v `seq` writeIORef ref v
modifyRef ref f = writeRef ref . f =<< readIORef ref


withBuffer :: Handle -> (Buffer -> IO a) -> IO a
withBuffer handle f = do
    inFile <- newIORef . fromInteger =<< hTell handle
    inBuffer <- newIORef 0
    patchup <- newIORef []
    allocaBytes (fromIntegral bufferSize) $ \ptr -> do
        res <- f $ Buffer handle ptr inFile inBuffer patchup
        inBuf <- readIORef inBuffer
        when (inBuf > 0) $ hPutBuf handle ptr (fromIntegral inBuf)
        xs <- fmap (sortOn $ \(a := b) -> a) $ readIORef patchup
        forM_ xs $ \(pos := val) -> do
            hSeek handle AbsoluteSeek $ toInteger pos
            poke (castPtr ptr) val
            hPutBuf handle ptr $ sizeOf val
        return res


put :: Buffer -> Word32 -> (Handle -> IO ()) -> (Ptr a -> Int -> IO ()) -> IO ()
put _ 0 _ _ = return ()
put Buffer{..} sz toFile toBuffer = do
    inBuf <- readIORef inBuffer
    if inBuf + sz >= bufferSize then do
        when (inBuf > 0) $ hPutBuf handle ptr $ fromIntegral inBuf
        if sz >= bufferSize `div` 2 then do
            toFile handle
            modifyRef inFile (+ (inBuf+sz))
            writeRef inBuffer 0
         else do
            toBuffer (castPtr ptr) 0
            modifyRef inFile (+inBuf)
            writeRef inBuffer sz
     else do
        toBuffer (castPtr ptr) $ fromIntegral inBuf
        writeIORef inBuffer (inBuf+sz)


putStorable :: Storable a => Buffer -> a -> IO ()
putStorable buf x = put buf (fromIntegral sz)
    (\h -> allocaBytes (sizeOf x) $ \ptr -> poke ptr x >> hPutBuf h ptr sz)
    (\ptr pos -> pokeByteOff ptr pos x)
    where sz = sizeOf x


putByteString :: Buffer -> BS.ByteString -> IO ()
putByteString buf x = put buf (fromIntegral $ BS.length x) (`BS.hPut` x) $
    \ptr pos -> let (fp,offset,len) = BS.toForeignPtr x in
                withForeignPtr fp $ \p -> BS.memcpy (plusPtr ptr pos) (plusPtr p offset) (fromIntegral len)


getPos :: Buffer -> IO Word32
getPos Buffer{..} = liftM2 (+) (readIORef inFile) (readIORef inBuffer)


-- Patch at position p, with value v. p must be in the past.
-- Return True if you succeeded, False if that is already on disk
patch :: Buffer -> Word32 -> Word32 -> IO ()
patch Buffer{..} p v = do
    i <- readIORef inFile
    if p >= i then
        pokeByteOff ptr (fromIntegral $ p-i) v
     else
        modifyRef patchup $ (:) (p := v)
