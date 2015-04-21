{-# LANGUAGE NoMonomorphismRestriction #-}

module General.Conduit(
    module Data.Conduit,
    sourceList, sinkList,
    foldC, mapC, mapMaybeC, mapAccumC, filterC, concatC,
    (|||),
    zipFromC, eitherC, countC, sumC, rightsC, awaitJust
    ) where

import Data.Conduit
import Data.Conduit.List as C
import Control.Applicative
import Control.Monad.Extra


concatC = C.concat
mapC = C.map
foldC = C.fold
mapMaybeC = C.mapMaybe
mapAccumC = C.mapAccum
filterC = C.filter

awaitJust act = do
    x <- await
    whenJust x act


zipFromC :: Monad m => Int -> Conduit a m (Int, a)
zipFromC i = awaitJust $ \a -> do
    yield (i,a)
    zipFromC (i+1)

eitherC :: Monad m => ConduitM i1 o m r1 -> ConduitM i2 o m r2 -> ConduitM (Either i1 i2) o m (r1,r2)
eitherC left right = (mapMaybeC l =$= left) ||| (mapMaybeC r =$= right)
    where l = either Just (const Nothing)
          r = either (const Nothing) Just

rightsC :: Monad m => ConduitM i2 o2 m () -> ConduitM (Either i1 i2) (Either i1 o2) m ()
rightsC c = void $ eitherC (mapC Left) (c =$= mapC Right)

countC :: (Monad m, Num c) => Sink a m c
countC = mapC (const 1) =$= sumC

sumC :: (Monad m, Num a) => Consumer a m a
sumC = foldC (+) 0

(|||) :: Monad m => ConduitM i o m r1 -> ConduitM i o m r2 -> ConduitM i o m (r1,r2)
(|||) a b = getZipConduit $ (,) <$> ZipConduit a <*> ZipConduit b

sinkList :: Monad m => Consumer a m [a]
sinkList = consume
