
module Data.Binary.Defer.Link(
    clearLinks, getLinks, putLinks,
    Link, newLink, fromLink, linkKey
    ) where

import Control.Monad.Trans
import General.Code
import Data.Binary.Defer
import Data.Binary.Defer.Array
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Typeable
import System.IO.Unsafe(unsafePerformIO)


data Links a = LinksMutable (IntMap.IntMap (Link a)) (Map.Map a (Link a))
             | LinksFrozen  (Array (Link a))


instance (Typeable a, BinaryDefer a) => BinaryDefer (Links a) where
    put (LinksFrozen xs) = put xs
    put x = put $ linksFreeze x
    get = liftM LinksFrozen get

instance BinaryDefer TypeRep where
    put = put . splitTyConApp
    get = liftM (uncurry mkTyConApp) get

instance BinaryDefer TyCon where
    put = put . tyConString
    get = liftM mkTyCon get


linksFreeze :: Links a -> Links a
linksFreeze (LinksMutable ks vs) = LinksFrozen $ array $ IntMap.elems ks

linksMutate :: Ord a => Links a -> Links a
linksMutate (LinksFrozen xs) = LinksMutable (IntMap.fromAscList kxs) (Map.fromList vxs)
    where (kxs,vxs) = unzip [(,) (linkKey e, e) (fromLink e, e) | e <- elems xs]


-- (Int <-> a) are a bijection
data Link a = Link Int a

instance Eq (Link a) where
    a == b = linkKey a == linkKey b

instance Ord a => Ord (Link a) where
    compare a b = compare (fromLink a) (fromLink b)


{-# NOINLINE links #-}
links :: IORef [(TypeRep,Links a)]
links = unsafePerformIO $ newIORef []



-- only required when being run from an interactive environment
-- i.e. Hugs or GHCi
clearLinks :: IO ()
clearLinks = writeIORef links []


putLinks :: (Typeable a, BinaryDefer a) => a -> DeferPut ()
putLinks a = do
    l <- lift $ readIORef links
    let (e,_) = adjustList (typeOf a) l
    put (typ e a)
    where
        typ :: Maybe (Links a) -> a -> Maybe (Links a)
        typ = const

getLinks :: (Typeable a, BinaryDefer a) => a -> DeferGet ()
getLinks a = do
    l <- lift $ readIORef links
    let (e,insert) = adjustList (typeOf a) l
    r <- get
    case (typ r a) of
        Nothing -> return ()
        Just r -> lift $ writeIORef links $ insert r
    where
        typ :: Maybe (Links a) -> a -> Maybe (Links a)
        typ = const


-- put the element you just modified at the front
adjustList :: Eq k => k -> [(k,v)] -> (Maybe v, v -> [(k,v)])
adjustList k [] = (Nothing, \v -> [(k,v)])
adjustList k ((k1,v1):kvs) | k == k1 = (Just v1, \v -> (k,v):kvs)
adjustList k kvs = (liftM snd $ listToMaybe yes, \v -> (k,v):no)
    where (yes,no) = partition ((==) k . fst) kvs


-- Insert a link, if you are working with LinksFrozen
-- ensure you turn it into a LinksMutable
newLink :: (Ord a, Typeable a) => a -> Link a
newLink a = unsafePerformIO $ do
    mp <- readIORef links
    let (e,insert) = adjustList (typeOf a) mp
        (e2,all2) = f e
    writeIORef links $ insert all2
    return e2
    where
        --f :: Maybe (Links a) -> (Link a, Links a)
        f Nothing = (res,mp)
            where res = Link 0 a
                  mp = LinksMutable (IntMap.singleton 0 res) (Map.singleton a res)

        f (Just l@(LinksMutable ks vs)) =
            case Map.lookup a vs of
                Just y -> (y,l)
                Nothing -> (res,mp)
                    where n = IntMap.size ks
                          res = Link n a
                          mp = LinksMutable (IntMap.insert n res ks) (Map.insert a res vs)

        f (Just l) = f $ Just $ linksMutate l


fromLink :: Link a -> a
fromLink (Link k v) = v


linkKey :: Link a -> Int
linkKey (Link k v) = k


instance (Typeable a, BinaryDefer a) => BinaryDefer (Link a) where
    put = put . linkKey

    get = res
        where
            res = liftM ask get
            t = typeOf $ typ res

            ask :: Int -> Link a
            ask = unsafePerformIO $ do
                mp <- readIORef links
                return $ case lookup t mp of
                    Nothing -> error "BinaryDefer.Link, woops!"
                    Just (LinksFrozen xs) -> (xs !)
                    Just (LinksMutable ks vs) -> (ks IntMap.!)

            typ :: DeferGet (Link a) -> a
            typ = undefined

    size _ = size (0 :: Int)
    putFixed = put
    getFixed = get
