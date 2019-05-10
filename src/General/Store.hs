{-# LANGUAGE DeriveDataTypeable, GADTs, PatternGuards, RecordWildCards,
             ScopedTypeVariables, ViewPatterns #-}

module General.Store(
    Typeable, Stored,
    intSize, intFromBS, intToBS, encodeBS, decodeBS,
    StoreWrite, storeWriteFile, storeWrite, storeWritePart,
    StoreRead, storeReadFile, storeRead,
    Jagged, jaggedFromList, jaggedAsk,
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Char
import Data.IORef.Extra
import Data.List.Extra
import qualified Data.Map as Map
import Data.Typeable
import qualified Data.Vector.Storable as V
import Data.Version
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import General.Util
import Numeric.Extra
import Paths_hoogle
import Prelude
import System.IO.Extra
import System.IO.MMap
import System.IO.Unsafe

-- Ensure the string is always 25 chars long, so version numbers don't change its size
-- Only use the first two components of the version number to identify the database
verString = BS.pack $ take 25 $ "HOOGLE-" ++ showVersion (trimVersion 3 version) ++ repeat ' '

---------------------------------------------------------------------
-- SERIALISATION HELPERS

intSize :: Int
intSize = 4

intToBS :: Int -> BS.ByteString
intToBS i = encodeBS (fromIntegral i :: Word32)

intFromBS :: BS.ByteString -> Int
intFromBS bs = fromIntegral (decodeBS bs :: Word32)

encodeBS :: Binary a => a -> BS.ByteString
encodeBS = LBS.toStrict . encode

decodeBS :: Binary a => BS.ByteString -> a
decodeBS = decode . LBS.fromStrict


---------------------------------------------------------------------
-- TREE INDEX STRUCTURE

-- each atom name is either unique (a scope) or "" (a list entry)
data Atom = Atom
    {atomType :: String -- Type that the atom contains (for sanity checking)
    ,atomPosition :: {-# UNPACK #-} !Int -- Position at which the atom starts in the file
    ,atomSize :: {-# UNPACK #-} !Int -- Number of bytes the value takes up
    } deriving Show

instance Binary Atom where
    put (Atom a b c) = put a >> put b >> put c
    get = liftA3 Atom get get get

---------------------------------------------------------------------
-- TYPE CLASS

class Typeable a => Stored a where
    storedWrite :: Typeable (t a) => StoreWrite -> t a -> Bool -> a -> IO ()
    storedRead :: Typeable (t a) => StoreRead -> t a -> a

instance Stored BS.ByteString where
    storedWrite store k part v = BS.unsafeUseAsCStringLen v $ \x -> storeWriteAtom store k part x
    storedRead store k = storeReadAtom store k BS.unsafePackCStringLen

instance forall a . (Typeable a, Storable a) => Stored (V.Vector a) where
    storedWrite store k part v = V.unsafeWith v $ \ptr ->
        storeWriteAtom store k part (castPtr ptr, V.length v * sizeOf (undefined :: a))
    storedRead store k = storeReadAtom store k $ \(ptr, len) -> do
        ptr <- newForeignPtr_ $ castPtr ptr
        return $ V.unsafeFromForeignPtr0 ptr (len `div` sizeOf (undefined :: a))


---------------------------------------------------------------------
-- WRITE OUT

data SW = SW
    {swHandle :: Handle -- Immutable handle I write to
    ,swPosition :: !Int -- Position within swHandle
    ,swAtoms :: [(String, Atom)] -- List of pieces, in reverse
    }

newtype StoreWrite = StoreWrite (IORef SW)

storeWriteFile :: FilePath -> (StoreWrite -> IO a) -> IO ([String], a)
storeWriteFile file act = do
    atoms <- newIORef Map.empty
    parts <- newIORef Nothing
    withBinaryFile file WriteMode $ \h -> do
        -- put the version string at the start and end, so we can tell truncation vs wrong version
        BS.hPut h verString
        ref <- newIORef $ SW h (BS.length verString) []
        res <- act $ StoreWrite ref
        SW{..} <- readIORef ref

        -- sort the atoms and validate there are no duplicates
        let atoms = Map.fromList swAtoms
        when (Map.size atoms /= length swAtoms) $
            errorIO "Some duplicate names have been written out"

        -- write the atoms out, then put the size at the end
        let bs = encodeBS atoms
        BS.hPut h bs
        BS.hPut h $ intToBS $ BS.length bs
        BS.hPut h verString

        final <- hTell h
        let stats = prettyTable 0 "Bytes" $
                ("Overheads", intToDouble $ fromIntegral final - sum (map atomSize $ Map.elems atoms)) :
                [(name ++ " :: " ++ atomType, intToDouble atomSize) | (name, Atom{..}) <- Map.toList atoms]
        return (stats, res)

storeWrite :: (Typeable (t a), Typeable a, Stored a) => StoreWrite -> t a -> a -> IO ()
storeWrite store k = storedWrite store k False

storeWritePart :: forall t a . (Typeable (t a), Typeable a, Stored a) => StoreWrite -> t a -> a -> IO ()
storeWritePart store k = storedWrite store k True

{-# NOINLINE putBuffer #-}
putBuffer a b c = hPutBuf a b c

storeWriteAtom :: forall t a . (Typeable (t a), Typeable a) => StoreWrite -> t a -> Bool -> CStringLen -> IO ()
storeWriteAtom (StoreWrite ref) (show . typeOf -> key) part (ptr, len) = do
    sw@SW{..} <- readIORef ref
    putBuffer swHandle ptr len

    let val = show $ typeRep (Proxy :: Proxy a)
    atoms <- case swAtoms of
        (keyOld,a):xs | part, key == keyOld -> do
            let size = atomSize a + len
            evaluate size
            return $ (key,a{atomSize=size}) : xs
        _ -> return $ (key, Atom val swPosition len) : swAtoms
    writeIORef' ref sw{swPosition = swPosition + len, swAtoms = atoms}


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
        errorIO $ "The Hoogle file " ++ file ++ " is corrupt, only " ++ show len ++ " bytes."

    let verN = BS.length verString
    verEnd <- BS.unsafePackCStringLen (plusPtr ptr $ len - verN, verN)
    when (verString /= verEnd) $ do
        verStart <- BS.unsafePackCStringLen (plusPtr ptr 0, verN)
        if verString /= verStart then
            errorIO $ "The Hoogle file " ++ file ++ " is the wrong version or format.\n" ++
                      "Expected: " ++ trim (BS.unpack verString) ++ "\n" ++
                      "Got     : " ++ map (\x -> if isAlphaNum x || x `elem` "_-. " then x else '?') (trim $ BS.unpack verStart)
         else
            errorIO $ "The Hoogle file " ++ file ++ " is truncated, probably due to an error during creation."

    atomSize <- intFromBS <$> BS.unsafePackCStringLen (plusPtr ptr $ len - verN - intSize, intSize)
    when (len < verN + intSize + atomSize) $
        errorIO $ "The Hoogle file " ++ file ++ " is corrupt, couldn't read atom table."
    atoms <- decodeBS <$> BS.unsafePackCStringLen (plusPtr ptr $ len - verN - intSize - atomSize, atomSize)
    act $ StoreRead file len ptr atoms

storeRead :: (Typeable (t a), Typeable a, Stored a) => StoreRead -> t a -> a
storeRead = storedRead


storeReadAtom :: forall a t . (Typeable (t a), Typeable a) => StoreRead -> t a -> (CStringLen -> IO a) -> a
storeReadAtom StoreRead{..} (typeOf -> k) unpack = unsafePerformIO $ do
    let key = show k
    let val = show $ typeRep (Proxy :: Proxy a)
    let corrupt msg = errorIO $ "The Hoogle file " ++ srFile ++ " is corrupt, " ++ key ++ " " ++ msg ++ "."
    case Map.lookup key srAtoms of
        Nothing -> corrupt "is missing"
        Just Atom{..}
            | atomType /= val -> corrupt $ "has type " ++ atomType ++ ", expected " ++ val
            | atomPosition < 0 || atomPosition + atomSize > srLen -> corrupt "has incorrect bounds"
            | otherwise -> unpack (plusPtr srPtr atomPosition, atomSize)

---------------------------------------------------------------------
-- PAIRS

newtype Fst k v where Fst :: k -> Fst k a deriving Typeable
newtype Snd k v where Snd :: k -> Snd k b deriving Typeable

instance (Typeable a, Typeable b, Stored a, Stored b) => Stored (a,b) where
    storedWrite store k False (a,b) = storeWrite store (Fst k) a >> storeWrite store (Snd k) b
    storedRead store k = (storeRead store $ Fst k, storeRead store $ Snd k)


---------------------------------------------------------------------
-- LITERALS

data StoredInt k v where StoredInt :: k -> StoredInt k BS.ByteString deriving Typeable

instance Stored Int where
    storedWrite store k False v = storeWrite store (StoredInt k) $ intToBS v
    storedRead store k = intFromBS $ storeRead store (StoredInt k)


---------------------------------------------------------------------
-- JAGGED ARRAYS

data Jagged a = Jagged (V.Vector Word32) (V.Vector a) deriving Typeable
data JaggedStore k v where JaggedStore :: k -> JaggedStore k (V.Vector Word32, V.Vector a) deriving Typeable

jaggedFromList :: Storable a => [[a]] -> Jagged a
jaggedFromList xs = Jagged is vs
    where is = V.fromList $ scanl (+) 0 $ map (\x -> fromIntegral $ length x :: Word32) xs
          vs = V.fromList $ concat xs

jaggedAsk :: Storable a => Jagged a -> Int -> V.Vector a
jaggedAsk (Jagged is vs) i = V.slice start (end - start) vs
    where start = fromIntegral $ is V.! i
          end   = fromIntegral $ is V.! succ i

instance (Typeable a, Storable a) => Stored (Jagged a) where
    storedWrite store k False (Jagged is vs) = storeWrite store (JaggedStore k) (is, vs)
    storedRead store k = uncurry Jagged $ storeRead store $ JaggedStore k
