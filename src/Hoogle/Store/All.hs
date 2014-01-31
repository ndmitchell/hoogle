{-# LANGUAGE ScopedTypeVariables #-}

module Hoogle.Store.All(
    SPut, SGet, runSPut, runSGet, runSGetAt, runAfter,
    Once, fromOnce, once, findOnce, unsafeFmapOnce, getDefer, putDefer,
    module Hoogle.Store.All
    ) where

import General.Base
import Foreign(sizeOf)
import Hoogle.Store.Type
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Array


class Store a where
    put :: a -> SPut ()
    get :: SGet a


    -- FIXME: unnecessary, just do an accumulator building up in reverse
    getList :: Int -> SGet [a]
    getList n = replicateM n get

    putList :: [a] -> SPut ()
    putList = mapM_ put

    size :: a -> Maybe Int -- may not look at the size argument
    size _ = Nothing


newtype Defer a = Defer {fromDefer :: a}

instance NFData a => NFData (Defer a) where rnf = rnf . fromDefer
instance Eq a => Eq (Defer a) where a == b = fromDefer a == fromDefer b
instance Ord a => Ord (Defer a) where compare a b = compare (fromDefer a) (fromDefer b)
instance Show a => Show (Defer a) where show = show . fromDefer

instance (Typeable a, Store a) => Store (Defer a) where
    put = putDefer . put . fromDefer
    get = fmap Defer $ getDefer get
    size _ = Just 4


instance Eq a => Eq (Once a) where a == b = fromOnce a == fromOnce b
instance Ord a => Ord (Once a) where compare a b = compare (fromOnce a) (fromOnce b)
instance Show a => Show (Once a) where show = show . fromOnce

instance (Typeable a, Store a) => Store (Once a) where
    put = putOnce put
    get = getOnce get
    size _ = Just 4


errorSGet :: String -> SGet a
errorSGet typ = error $ "Store.get(" ++ typ ++ "), corrupt database"


get0 f = return f
get1 f = do x1 <- get; return (f x1)
get2 f = do x1 <- get; x2 <- get; return (f x1 x2)
get3 f = do x1 <- get; x2 <- get; x3 <- get; return (f x1 x2 x3)
get4 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; return (f x1 x2 x3 x4)
get5 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; return (f x1 x2 x3 x4 x5)
get6 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; return (f x1 x2 x3 x4 x5 x6)
get7 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; return (f x1 x2 x3 x4 x5 x6 x7)
get8 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; return (f x1 x2 x3 x4 x5 x6 x7 x8)
get9 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9)


put0 = return () :: SPut ()
put1 x1 = put x1
put2 x1 x2 = put x1 >> put x2
put3 x1 x2 x3 = put x1 >> put x2 >> put x3
put4 x1 x2 x3 x4 = put x1 >> put x2 >> put x3 >> put x4
put5 x1 x2 x3 x4 x5 = put x1 >> put x2 >> put x3 >> put x4 >> put x5
put6 x1 x2 x3 x4 x5 x6 = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6
put7 x1 x2 x3 x4 x5 x6 x7 = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6 >> put x7
put8 x1 x2 x3 x4 x5 x6 x7 x8 = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6 >> put x7 >> put x8
put9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6 >> put x7 >> put x8 >> put x9


putByte :: Word8 -> SPut (); putByte = put
getByte :: SGet Word8; getByte = get
putWord32 :: Word32 -> SPut (); putWord32 = put
getWord32 :: SGet Word32; getWord32 = get


instance Store Word8 where
    put = putStorable
    get = getStorable
    size = Just . sizeOf

instance Store Word32 where
    put = putStorable
    get = getStorable
    size = Just . sizeOf

instance Store Int32 where
    put = putStorable
    get = getStorable
    size = Just . sizeOf

instance Store Int where
    put x = putStorable (fromIntegral x :: Int32)
    get = fmap fromIntegral (getStorable :: SGet Int32)
    size _ = size (0 :: Int32)

instance Store Char where
    put x | x < '\x80' = putByte . fromIntegral . ord $ x -- ASCII
          | otherwise  = putByteString . T.encodeUtf8 . T.singleton $ x
    get = do c0 <- getByte
             n <- case c0 of
               _ | c0 < 0x80 -> return 0 -- ASCII
               _ | c0 < 0xc0 -> fail "invalid UTF8 sequence"
               _ | c0 < 0xe0 -> return 1
               _ | c0 < 0xf0 -> return 2
               _ | c0 < 0xf8 -> return 3
               _ | c0 < 0xfc -> return 4
               _ | c0 < 0xfe -> return 5

             if n > 0
              then fmap (T.head . T.decodeUtf8 . BS.cons c0) $ getByteString n
              else return $ chr $ fromIntegral $ c0 -- ASCII

    putList = putByteString . T.encodeUtf8 . T.pack

instance Store Bool where
    put x = put $ if x then '1' else '0'
    get = fmap (== '1') get
    size _ = size '1'

instance Store () where
    put () = return ()
    get = return ()
    size _ = Just 0

instance (Store a, Store b) => Store (a,b) where
    put (a,b) = put2 a b
    get = get2 (,)
    size ~(a,b) = liftM2 (+) (size a) (size b)

instance (Store a, Store b, Store c) => Store (a,b,c) where
    put (a,b,c) = put3 a b c
    get = get3 (,,)
    size ~(a,b,c) = liftM3 (\a b c -> a + b + c) (size a) (size b) (size c)

instance Store a => Store (Maybe a) where
    put Nothing = putByte 0
    put (Just a) = putByte 1 >> put a

    get = do i <- getByte
             case i of
                0 -> get0 Nothing
                1 -> get1 Just
                _ -> errorSGet "Maybe"

instance (Store a, Store b) => Store (Either a b) where
    put (Left a) = putByte 0 >> put a
    put (Right a) = putByte 1 >> put a

    get = do i <- getByte
             case i of
                0 -> get1 Left
                1 -> get1 Right
                _ -> errorSGet "Either"


-- strategy: write out a byte, 255 = length is an int, anything else = len
instance Store a => Store [a] where
    put xs = do
        let n = fromIntegral (length xs)
        let mx = maxBound :: Word8
        if n >= fromIntegral mx then putByte mx >> putWord32 n else putByte (fromIntegral n)
        putList xs

    get = do
        n <- getByte
        n <- if n == maxBound then getWord32 else return $ fromIntegral n
        getList $ fromIntegral n


instance Store BS.ByteString where
    put x = do
        putWord32 $ fromIntegral $ BS.length x
        putByteString x
    get = do
        n <- getWord32
        getByteString n


instance (Ix i, Store i, Store e) => Store (Array i e) where
    put x = do
        put $ bounds x
        putList $ elems x

    get = do
        bnd <- get
        fmap (listArray bnd) $ case size (undefined :: e) of
            Nothing -> getList $ rangeSize bnd
            Just sz -> getLazyList get sz (rangeSize bnd)


instance (Typeable k, Typeable v, Ord k, Store k, Store v) => Store (Map.Map k v) where
    put = putDefer . put . Prelude.map (second Defer) . Map.toAscList

    get = getDefer $ fmap (Map.fromAscList . Prelude.map (second fromDefer)) get
