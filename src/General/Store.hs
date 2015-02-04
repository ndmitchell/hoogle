{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings, PatternGuards #-}

module General.Store(
    Typeable, intSize, intFromBS, intToBS, encodeBS, decodeBS,
    StoreOut, writeStoreFile, writeStoreType, writeStoreParts, writeStoreBS, writeStoreV,
    StoreIn, readStoreFile, readStoreType, readStoreList, readStoreBS, readStoreV
    ) where

import Data.IORef
import System.IO.Extra
import Data.Typeable
import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Extra
import Data.Binary
import Data.List
import System.IO.MMap
import Control.Applicative
import System.IO.Unsafe
import General.Util
import Control.DeepSeq
import Control.Exception.Extra


---------------------------------------------------------------------
-- SERIALISATION HELPERS

intSize :: Int
intSize = 4

intToBS :: Int -> BS.ByteString
intToBS i = encodeBS (fromIntegral i :: Word32)

intFromBS :: BS.ByteString -> Int
intFromBS bs = fromIntegral (decodeBS bs :: Word32)

encodeBS :: Binary a => a -> BS.ByteString
encodeBS = BS.concat . LBS.toChunks . encode

decodeBS :: Binary a => BS.ByteString -> a
decodeBS = decode . LBS.fromChunks . return


---------------------------------------------------------------------
-- TREE INDEX STRUCTURE

-- each atom name is either unique (a scope) or "" (a list entry)
data Atom = Atom
    {atomName :: [String]
    ,atomPosition :: !Int
    ,atomCount :: !Int
    ,atomSize :: !Int
    } deriving Show

instance Binary Atom where
    put (Atom a b c d) = put a >> put b >> put c >> put d
    get = liftM4 Atom get get get get


---------------------------------------------------------------------
-- WRITE OUT

data StoreOut = StoreOut
    {storePrefix :: IORef [String] -- the current prefix
    ,storeAtoms :: IORef [Atom] -- the atoms that have been stored
    ,storeParts :: IORef Bool -- am I currently storing a part (part is first in storeAtoms)
    ,storeHandle :: Handle
    }

writeStoreFile :: FilePath -> (StoreOut -> IO a) -> IO a
writeStoreFile file act = do
    prefix <- newIORef []
    atoms <- newIORef []
    parts <- newIORef False
    withBinaryFile file WriteMode $ \h -> do
        res <- act $ StoreOut prefix atoms parts h
        -- write the atoms out, probably using binary, then put the size at the end
        atoms <- readIORef atoms
        let bs = encodeBS $ reverse atoms
        BS.hPut h bs
        BS.hPut h $ intToBS $ BS.length bs
        return res

notParts :: IORef Bool -> IO a -> IO a
notParts ref act = do
    whenM (readIORef ref) $ error "Not allowed to be storing parts"
    act

writeStorePtr :: forall a . Storable a => StoreOut -> Ptr a -> Int -> IO ()
writeStorePtr StoreOut{..} ptr len = do
    parts <- readIORef storeParts
    let size = sizeOf (undefined :: a)
    if parts then do
        a:as <- readIORef storeAtoms
        when (atomSize a `notElem` [0, size]) $ error "Writing parts, but atom size has changed"
        writeIORef storeAtoms $ a{atomSize=size, atomCount=atomCount a + len} : as
     else do
        tell <- hTell storeHandle
        prefix <- readIORef storePrefix
        modifyIORef storeAtoms (Atom prefix (fromInteger tell) len size:)
    hPutBuf storeHandle ptr $ len * size

writeStoreBS :: StoreOut -> BS.ByteString -> IO ()
writeStoreBS s bs = BS.unsafeUseAsCStringLen bs $ \(ptr,len) -> writeStorePtr s ptr len

writeStoreV :: Storable a => StoreOut -> Vector.Vector a -> IO ()
writeStoreV s v = Vector.unsafeWith v $ \ptr -> writeStorePtr s ptr $ Vector.length v

writeStoreParts :: StoreOut -> IO a -> IO a
writeStoreParts StoreOut{..} act = notParts storeParts $ do
    prefix <- readIORef storePrefix
    tell <- hTell storeHandle
    modifyIORef storeAtoms (Atom prefix 0 (fromIntegral tell) 0 :)
    writeIORef storeParts True
    res <- act
    writeIORef storeParts False
    return res

writeStoreType :: Typeable t => StoreOut -> t -> IO a -> IO a
writeStoreType StoreOut{..} t act = notParts storeParts $ do
    prefix <- readIORef storePrefix
    let name = prefix ++ [show $ typeOf t]
    atoms <- readIORef storeAtoms
    when (name `elem` map atomName atoms) $ error $ "Duplicate atom name, " ++ show name
    writeIORef storePrefix name
    res <- act
    modifyIORef storePrefix init
    return res


---------------------------------------------------------------------
-- READ OUT

data StoreIn = StoreIn (Ptr ()) [Atom] -- atoms are filtered by readStoreType

readStoreFile :: NFData a => FilePath -> (StoreIn -> IO a) -> IO a
readStoreFile file act = mmapWithFilePtr file ReadOnly Nothing $ \(ptr, len) -> do
    n <- intFromBS <$> BS.unsafePackCStringLen (plusPtr ptr $ len - intSize, intSize)
    atoms <- decodeBS <$> BS.unsafePackCStringLen (plusPtr ptr $ len - intSize - n, n)
    res <- try_ $ act $ StoreIn ptr atoms
    case res of
        Left e -> error' =<< showException e
        Right v -> do evaluate $ rnf v; return v

readStoreList :: StoreIn -> [StoreIn]
readStoreList (StoreIn ptr xs) = map (StoreIn ptr . return) $ filter (null . atomName) xs

readStoreType :: Typeable t => t -> StoreIn -> StoreIn
readStoreType t (StoreIn ptr atoms)
    | null good = error $ "Couldn't find atom with name " ++ name
    | otherwise = StoreIn ptr [a{atomName = tail $ atomName a} | a <- good]
    where
        name = show $ typeOf t
        good = filter (isPrefixOf [name] . atomName) atoms

readStoreBS :: StoreIn -> BS.ByteString
readStoreBS (StoreIn ptr atoms)
    | [Atom{..}] <- good, atomSize == 1 = unsafePerformIO $ BS.unsafePackCStringLen (plusPtr ptr atomPosition, atomCount)
    | otherwise = error "bad BS"
    where good = filter (null . atomName) atoms

readStoreV :: forall a . Storable a => StoreIn -> Vector.Vector a
readStoreV (StoreIn ptr atoms)
    | [Atom{..}] <- good, atomSize == sizeOf (undefined :: a) = unsafePerformIO $ do
        ptr <- newForeignPtr_ $ plusPtr ptr atomPosition
        return $ Vector.unsafeFromForeignPtr0 ptr atomCount
    | otherwise = error $ "bad vector, " ++ show (map atomSize good)
    where good = filter (null . atomName) atoms
