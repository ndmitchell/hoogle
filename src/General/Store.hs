{-# LANGUAGE ScopedTypeVariables, RecordWildCards, PatternGuards #-}

module General.Store(
    Typeable, intSize, intFromBS, intToBS, encodeBS, decodeBS,
    StoreWrite, storeWriteFile, storeWriteType, storeWriteParts, storeWriteBS, storeWriteV,
    StoreRead, storeReadFile, storeReadType, storeReadList, storeReadBS, storeReadV
    ) where

import Data.IORef
import System.IO.Extra
import Data.Typeable
import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Extra
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

data StoreWrite = StoreWrite
    {swPrefix :: IORef [String] -- the current prefix
    ,swAtoms :: IORef [Atom] -- the atoms that have been stored
    ,swParts :: IORef Bool -- am I currently storing a part (part is first in swAtoms)
    ,swHandle :: Handle
    }

storeWriteFile :: FilePath -> (StoreWrite -> IO a) -> IO a
storeWriteFile file act = do
    prefix <- newIORef []
    atoms <- newIORef []
    parts <- newIORef False
    withBinaryFile file WriteMode $ \h -> do
        -- put the version string at the start and end, so we can tell truncation vs wrong version
        BS.hPut h verString
        res <- act $ StoreWrite prefix atoms parts h
        -- write the atoms out, probably using binary, then put the size at the end
        atoms <- readIORef atoms
        let bs = encodeBS $ reverse atoms
        BS.hPut h bs
        BS.hPut h $ intToBS $ BS.length bs
        BS.hPut h verString
        return res

notParts :: IORef Bool -> IO a -> IO a
notParts ref act = do
    whenM (readIORef ref) $ error "Not allowed to be storing parts"
    act

storeWritePtr :: forall a . Storable a => StoreWrite -> Ptr a -> Int -> IO ()
storeWritePtr StoreWrite{..} ptr len = do
    parts <- readIORef swParts
    let size = sizeOf (undefined :: a)
    if parts then do
        a:as <- readIORef swAtoms
        when (atomSize a `notElem` [0, size]) $ error "Writing parts, but atom size has changed"
        writeIORef swAtoms $ a{atomSize=size, atomCount=atomCount a + len} : as
     else do
        tell <- hTell swHandle
        prefix <- readIORef swPrefix
        modifyIORef swAtoms (Atom prefix (fromInteger tell) len size:)
    hPutBuf swHandle ptr $ len * size

storeWriteBS :: StoreWrite -> BS.ByteString -> IO ()
storeWriteBS s bs = BS.unsafeUseAsCStringLen bs $ \(ptr,len) -> storeWritePtr s ptr len

storeWriteV :: Storable a => StoreWrite -> Vector.Vector a -> IO ()
storeWriteV s v = Vector.unsafeWith v $ \ptr -> storeWritePtr s ptr $ Vector.length v

storeWriteParts :: StoreWrite -> IO a -> IO a
storeWriteParts StoreWrite{..} act = notParts swParts $ do
    prefix <- readIORef swPrefix
    tell <- hTell swHandle
    modifyIORef swAtoms (Atom prefix (fromIntegral tell) 0 0 :)
    writeIORef swParts True
    res <- act
    writeIORef swParts False
    return res

storeWriteType :: Typeable t => StoreWrite -> t -> IO a -> IO a
storeWriteType StoreWrite{..} t act = notParts swParts $ do
    prefix <- readIORef swPrefix
    let name = prefix ++ [show $ typeOf t]
    atoms <- readIORef swAtoms
    when (name `elem` map atomName atoms) $ error $ "Duplicate atom name, " ++ show name
    writeIORef swPrefix name
    res <- act
    modifyIORef swPrefix init
    return res


---------------------------------------------------------------------
-- READ OUT

data StoreRead = StoreRead
    {srFile :: FilePath
    ,srLen :: Int
    ,srPtr :: Ptr ()
    ,srAtoms :: [Atom] -- filtered and name prefix stripped list of atoms
    ,srPrefix :: [String] -- grows as we descend
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
    act $ StoreRead file len ptr atoms []

storeReadList :: StoreRead -> [StoreRead]
storeReadList sr@StoreRead{..} =
    [sr{srAtoms=[a], srPrefix=srPrefix++["@" ++ show i]} | (i,a) <- zip [0..] $ filter (null . atomName) srAtoms]

storeReadType :: Typeable t => t -> StoreRead -> StoreRead
storeReadType t sr@StoreRead{..}
    | null found = error $ "The Hoogle file " ++ srFile ++ " is corrupt, no atoms named " ++ concatMap (++".") (srPrefix++[name])
    | otherwise = sr{srAtoms=found, srPrefix=srPrefix++[name]}
    where
        name = show $ typeOf t
        found = [a{atomName=xs} | a@Atom{atomName=x:xs} <- srAtoms, x == name]

storeReadAtom :: StoreRead -> Int -> (Ptr a, Int)
storeReadAtom StoreRead{..} size = case filter (null . atomName) srAtoms of
    [Atom{..}] ->
        if atomSize /= size then
            error $ "The Hoogle file " ++ srFile ++ " is corrupt, incorrect atom size for " ++ name ++ "."
        else if atomPosition < 0 || atomPosition + (atomSize * atomCount) > srLen then
            error $ "The hoogle file " ++ srFile ++ " is corrupt, incorrect bounds for " ++ name ++ "."
        else
            (plusPtr srPtr atomPosition, atomCount)
    [] -> error $ "The Hoogle file " ++ srFile ++ " is corrupt, no atom named " ++ name ++ "."
    xs -> error $ "The Hoogle file " ++ srFile ++ " is corrupt, " ++ show (length xs) ++ " atoms named " ++ name ++ ", expected 1."
    where name = intercalate "." srPrefix

storeReadBS :: StoreRead -> BS.ByteString
storeReadBS sr = unsafePerformIO $ BS.unsafePackCStringLen $ storeReadAtom sr 1

storeReadV :: forall a . Storable a => StoreRead -> Vector.Vector a
storeReadV sr = unsafePerformIO $ do
    let (ptr, len) = storeReadAtom sr $ sizeOf (undefined :: a)
    ptr <- newForeignPtr_ ptr
    return $ Vector.unsafeFromForeignPtr0 ptr len
