{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Hoogle.Store.Type(
    Once, once, fromOnce, putOnce, getOnce, findOnce, unsafeFmapOnce,
    SPut, runSPut, putByteString, putStorable, putDefer, runAfter,
    SGet, runSGet, runSGetAt, getByteString, getStorable, getDefer, getLazyList
    ) where

import General.Base
import General.System
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Typeable
import Foreign
import System.IO.Unsafe
import qualified Hoogle.Store.ReadBuffer as R
import qualified Hoogle.Store.WriteBuffer as W


-- | Turn on to see file statistics
stats = False


-- | All once values are equal with respect to keyOnce
--   If you create it with 'once' it will have the same key.
--   If two are loaded from a file they are equal.
data Once a = Once {_onceKey :: Int, valueOnce :: a}
              deriving Typeable

unsafeFmapOnce :: (a -> b) -> Once a -> Once b
unsafeFmapOnce f (Once k v) = Once k $ f v

instance NFData a => NFData (Once a) where
    rnf (Once a b) = rnf (a,b)

fromOnce :: Once a -> a
fromOnce = valueOnce

-- | Given how many you would like to allocate, return your base address
onceKeys :: Int -> IO Int
onceKeys = System.IO.Unsafe.unsafePerformIO $ do
    ref <- newIORef 0
    return $ \n -> atomicModifyIORef ref $ \x -> (x+n, x)


---------------------------------------------------------------------
-- PUT

data SPutS = SPutS
    {putBuffer :: W.Buffer
    ,putOnces :: IORef (IntMap.IntMap PutOnce)
    ,putPending :: IORef [SPut ()]
    }

type SPut a = ReaderT SPutS IO a

modifyRef f x = liftIO . (`modifyIORef` x) =<< asks f
readPos = liftIO . W.getPos =<< asks putBuffer


runSPut :: FilePath -> SPut a -> IO a
runSPut file act = withBinaryFile file WriteMode $ \h -> do
    pending <- newIORef []
    once <- newIORef IntMap.empty

    W.withBuffer h $ \buffer -> do
        let flush = do
                xs <- liftIO $ readIORef pending
                liftIO $ writeIORef pending []
                forM_ xs $ \x -> do
                    x
                    flush
        runReaderT (do res <- act; flush; return res) $ SPutS buffer once pending


putByteString :: BString -> SPut ()
putByteString x = do
    buf <- asks putBuffer
    liftIO $ W.putByteString buf x

putStorable :: Storable a => a -> SPut ()
putStorable x = do
    buf <- asks putBuffer
    liftIO $ W.putStorable buf x


putDefer :: SPut () -> SPut ()
putDefer act = do
    pos <- readPos
    putStorable (0 :: Word32)
    modifyRef putPending $ (:) $ do
        val <- readPos
        buf <- asks putBuffer
        liftIO $ W.patch buf pos val
        act

runAfter :: SPut () -> SPut ()
runAfter act = modifyRef putPending (++[act])


{-# NOINLINE once #-}
once :: a -> Once a
once x = System.IO.Unsafe.unsafePerformIO $ do
    key <- onceKeys 1
    return $ Once key x


type PutOnce = Either [Word32] Word32

findOnce :: Once a -> SPut (Maybe Word32)
findOnce (Once key _) = do
    ref <- asks putOnces
    mp <- liftIO $ readIORef ref
    return $ case IntMap.lookup key mp of
        Just (Right val) -> Just val
        _ -> Nothing

putOnce :: (a -> SPut ()) -> Once a -> SPut ()
putOnce act (Once key x) = do
    ref <- asks putOnces
    mp <- liftIO $ readIORef ref
    case fromMaybe (Left []) $ IntMap.lookup key mp of
        -- written out at this address
        Right val -> putStorable val

        -- [] is has not been added to the defer list
        -- (:) is on defer list but not yet written, these are places that need back patching
        Left poss -> do
            pos <- readPos
            liftIO $ writeIORef ref $ IntMap.insert key (Left $ pos:poss) mp
            putStorable (0 :: Word32)
            when (null poss) $ modifyRef putPending $ (:) $ do
                val <- readPos
                mp <- liftIO $ readIORef ref
                let Left poss = mp IntMap.! key
                buf <- asks putBuffer
                liftIO $ forM_ poss $ \pos -> W.patch buf pos val
                liftIO $ writeIORef ref $ IntMap.insert key (Right val) mp
                act x


---------------------------------------------------------------------
-- GET

-- getPtr is the pointer you have, how much is left valid, 
data SGetS = SGetS {getBuffer :: R.Buffer, onceBase :: Int}

type SGet a = ReaderT SGetS IO a


runSGet :: Typeable a => FilePath -> SGet a -> IO a
runSGet = runSGetAt 0

runSGetAt :: Typeable a => Word32 -> FilePath -> SGet a -> IO a
runSGetAt pos file m = do
    h <- openBinaryFile file ReadMode
    sz <- hFileSize h
    buf <- R.newBuffer h
    one <- onceKeys $ fromIntegral sz
    runReaderT (getDeferFrom pos m) $ SGetS buf one


getStorable :: Typeable a => Storable a => SGet a
getStorable = do
    buf <- asks getBuffer
    res <- liftIO $ R.getStorable buf
    when stats $ liftIO $ putStrLn $ "Reading storable " ++ show (sizeOf res)
    return res


getByteString :: Word32 -> SGet BString
getByteString len = do
    buf <- asks getBuffer
    when stats $ liftIO $ putStrLn $ "Reading bytestring " ++ show len
    liftIO $ R.getByteString buf $ fromIntegral len


getDefer :: Typeable a => SGet a -> SGet a
getDefer get = do
    pos :: Word32 <- getStorable
    getDeferFrom pos get


getDeferFrom :: forall a . Typeable a => Word32 -> SGet a -> SGet a
getDeferFrom pos get = do
    s <- ask
    liftIO $ unsafeInterleaveIO $ do
        when stats $ putStrLn $ "Read at " ++ show (typeOf (undefined :: a))
        R.setPos (getBuffer s) pos
        runReaderT get s


getOnce :: Typeable a => SGet a -> SGet (Once a)
getOnce get = do
    pos :: Word32 <- getStorable
    x <- getDeferFrom pos get
    one <- asks onceBase
    return $ Once (fromIntegral pos + one) x


getLazyList :: SGet a -> Int -> Int -> SGet [a]
getLazyList get size n = do
    s <- ask
    pos <- liftIO $ R.getPos $ getBuffer s
    liftIO $ forM [0..n-1] $ \i -> unsafeInterleaveIO $ do
        R.setPos (getBuffer s) (pos + fromIntegral (i * size))
        runReaderT get s
