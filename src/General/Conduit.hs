{-# LANGUAGE NoMonomorphismRestriction #-}

module General.Conduit(
    module Data.Conduit, MonadIO, liftIO,
    sourceList, sinkList,
    foldC, mapC, mapMaybeC, mapAccumC, filterC, concatC,
    (|$|), (|>), (<|),
    zipFromC, eitherC, countC, sumC, rightsC, awaitJust, linesC
    ) where

import Data.Conduit
import Data.Conduit.List as C
import Control.Applicative
import Control.Monad.Extra
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import Prelude


(|>) = (=$=)
(<|) = flip (|>)

concatC = C.concat
mapC = C.map
foldC = C.fold
mapMaybeC = C.mapMaybe
mapAccumC f = C.mapAccum (\a s -> f s a)
filterC = C.filter

awaitJust :: Monad m => (i -> Conduit i m o) -> Conduit i m o
awaitJust act = do
    x <- await
    whenJust x act

zipFromC :: (Monad m, Enum c) => c -> Conduit a m (c, a)
zipFromC i = awaitJust $ \a -> do
    yield (i,a)
    zipFromC (succ i)

eitherC :: Monad m => ConduitM i1 o m r1 -> ConduitM i2 o m r2 -> ConduitM (Either i1 i2) o m (r1,r2)
eitherC left right = (mapMaybeC l |> left) |$| (mapMaybeC r |> right)
    where l = either Just (const Nothing)
          r = either (const Nothing) Just

rightsC :: Monad m => ConduitM i2 o2 m () -> ConduitM (Either i1 i2) (Either i1 o2) m ()
rightsC c = void $ eitherC (mapC Left) (c |> mapC Right)

countC :: (Monad m, Num c) => Sink a m c
countC = sumC <| mapC (const 1)

sumC :: (Monad m, Num a) => Consumer a m a
sumC = foldC (+) 0

(|$|) :: Monad m => ConduitM i o m r1 -> ConduitM i o m r2 -> ConduitM i o m (r1,r2)
(|$|) a b = getZipConduit $ (,) <$> ZipConduit a <*> ZipConduit b

sinkList :: Monad m => Consumer a m [a]
sinkList = consume


linesC :: Monad m => Conduit BS.ByteString m BS.ByteString
linesC = loop []
    where
        loop acc = await >>= maybe (finish acc) (go acc)

        finish acc = unless (BS.null final) (yield final)
            where final = BS.concat $ reverse acc

        go acc more = case BS.uncons second of
            Just (_, second') -> yield (BS.concat $ reverse $ first:acc) >> go [] second'
            Nothing -> loop $ more:acc
            where (first, second) = BS.break (== '\n') more
