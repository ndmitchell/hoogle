{-# LANGUAGE ScopedTypeVariables, RecordWildCards, PatternGuards #-}

module General.Store(
    Typeable, Stored,
    intSize, intFromBS, intToBS,
    StoreWrite, storeWriteFile, storeWrite, storeWritePart,
    StoreRead, storeReadFile, storeRead
    ) where

import Data.IORef
import System.IO.Extra
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Extra
import Control.Exception
import Data.Binary
import Data.List.Extra
import System.IO.MMap
import Control.Applicative
import System.IO.Unsafe
import General.Util
import Control.DeepSeq
import Data.Version
import Data.Char
import Paths_hoogle
import Prelude

-- ensure the string is always 25 chars long, so version numbers don't change its size
verString = BS.pack $ take 25 $ "HOOGLE-" ++ showVersion version ++ repeat ' '

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
    {atomType :: String -- Type that the atom contains (for sanity checking)
    ,atomPosition :: !Int -- Position at which the atom starts in the file
    ,atomSize :: !Int -- Number of bytes the value takes up
    } deriving Show

instance Binary Atom where
    put (Atom a b c) = put a >> put b >> put c
    get = liftM3 Atom get get get

---------------------------------------------------------------------
-- TYPE CLASS

class Stored a where
    -- Convert between a value and a size/memory-block
    -- storedWrite must keep hold of the memory while writing
    -- storedRead has its memory kept externally
    storedWrite :: a -> (CStringLen -> IO b) -> IO b
    storedRead :: CStringLen -> IO a

instance Stored BS.ByteString where
    storedWrite bs op = BS.unsafeUseAsCStringLen bs op
    storedRead = BS.unsafePackCStringLen

instance forall a . Storable a => Stored (Vector.Vector a) where
    storedWrite v op = Vector.unsafeWith v $ \ptr -> op (castPtr ptr, Vector.length v * sizeOf (undefined :: a))
    storedRead (ptr, len) = do
        ptr <- newForeignPtr_ $ castPtr ptr
        return $ Vector.unsafeFromForeignPtr0 ptr (len `div` sizeOf (undefined :: a))


---------------------------------------------------------------------
-- WRITE OUT

data StoreWrite = StoreWrite
    {swAtoms :: IORef (Map.Map String Atom) -- the atoms that have been stored
    ,swPart :: IORef (Maybe String) -- am I currently storing a part
    ,swHandle :: Handle
    }

storeWriteFile :: FilePath -> (StoreWrite -> IO a) -> IO a
storeWriteFile file act = do
    atoms <- newIORef Map.empty
    parts <- newIORef Nothing
    withBinaryFile file WriteMode $ \h -> do
        -- put the version string at the start and end, so we can tell truncation vs wrong version
        BS.hPut h verString
        res <- act $ StoreWrite atoms parts h
        -- write the atoms out, then put the size at the end
        atoms <- readIORef atoms
        let bs = encodeBS atoms
        BS.hPut h bs
        BS.hPut h $ intToBS $ BS.length bs
        BS.hPut h verString
        return res

storeWrite :: (Typeable t, Typeable a, Stored a) => StoreWrite -> t a -> a -> IO ()
storeWrite store@StoreWrite{..} k v = do
    writeIORef swPart Nothing
    storeWritePart store k v
    writeIORef swPart Nothing

storeWritePart :: (Typeable t, Typeable a, Stored a) => StoreWrite -> t a -> a -> IO ()
storeWritePart StoreWrite{..} k v = do
    start <- fromIntegral <$> hTell swHandle
    len <- storedWrite v $ \(ptr, len) -> do hPutBuf swHandle ptr len >> return len

    let key = show $ typeOf k
    let val = show $ typeOf v
    atoms <- readIORef swAtoms
    part <- readIORef swPart
    if part == Just key then do
        let upd a | start /= atomPosition a + atomSize a = error "Internal error, inconsistent storeWritePart"
                  | atomType a /= val = error "Different type when doing subsequent storeWritePart"
                  | otherwise = a{atomSize = atomSize a + len}
        let atom2 = upd $ atoms Map.! key
        evaluate atom2
        writeIORef swAtoms $ Map.insert key atom2 atoms
    else if key `Map.member` atoms then
        error "Duplicate key name in storeWritePart"
    else do
        writeIORef swAtoms $ Map.insert key (Atom val start len) atoms
        writeIORef swPart $ Just key


---------------------------------------------------------------------
-- READ OUT

data StoreRead = StoreRead
    {srFile :: FilePath
    ,srLen :: Int
    ,srPtr :: Ptr ()
    ,srAtoms :: Map.Map String Atom
    }

storeReadFile :: NFData a => FilePath -> (StoreRead -> IO a) -> IO a
storeReadFile file act = mmapWithFilePtr file ReadOnly Nothing $ \(ptr, len) -> strict $ do
    -- check is longer than my version string
    when (len < (BS.length verString * 2) + intSize) $
        error $ "The Hoogle file " ++ file ++ " is corrupt, only " ++ show len ++ " bytes."

    let verN = BS.length verString
    verEnd <- BS.unsafePackCStringLen (plusPtr ptr $ len - verN, verN)
    when (verString /= verEnd) $ do
        verStart <- BS.unsafePackCStringLen (plusPtr ptr 0, verN)
        if verString /= verStart then
            error $ "The Hoogle file " ++ file ++ " is the wrong version or format.\n" ++
                    "Expected: " ++ trim (BS.unpack verString) ++ "\n" ++
                    "Got     : " ++ map (\x -> if isAlphaNum x || x `elem` "_-. " then x else '?') (trim $ BS.unpack verStart)
         else
            error $ "The Hoogle file " ++ file ++ " is truncated, probably due to an error during creation."

    atomSize <- intFromBS <$> BS.unsafePackCStringLen (plusPtr ptr $ len - verN - intSize, intSize)
    when (len < verN + intSize + atomSize) $
        error $ "The Hoogle file " ++ file ++ " is corrupt, couldn't read atom table."
    atoms <- decodeBS <$> BS.unsafePackCStringLen (plusPtr ptr $ len - verN - intSize - atomSize, atomSize)
    act $ StoreRead file len ptr atoms


storeRead :: forall a t . (Typeable t, Typeable a, Stored a) => StoreRead -> t a -> a
storeRead StoreRead{..} k = unsafePerformIO $ do
    let key = show $ typeOf k
    let val = show $ typeOf (undefined :: a)
    let corrupt msg = error $ "The Hoogle file " ++ srFile ++ " is corrupt, " ++ key ++ " " ++ msg ++ "."
    case Map.lookup key srAtoms of
        Nothing -> corrupt "is missing"
        Just Atom{..}
            | atomType /= val -> corrupt $ "has type " ++ atomType ++ ", expected " ++ val
            | atomPosition < 0 || atomPosition + atomSize > srLen -> corrupt "has incorrect bounds"
            | otherwise -> storedRead (plusPtr srPtr atomPosition, atomSize)
