{-# LANGUAGE NoMonomorphismRestriction, PatternGuards, Rank2Types, CPP, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- QSem was deprecated in 7.6, but then undeprecated

module General.Conduit(
    module Data.Conduit, MonadIO, liftIO,
    sourceList, sinkList, sourceLStr,
    foldC, mapC, mapMaybeC, mapAccumC, filterC, concatC,
    (|$|), (|>), (<|), pipelineC,
    zipFromC, eitherC, countC, sumC, rightsC, awaitJust, linesC, linesCR
    ) where

import Data.Conduit
import Data.Conduit.List as C
import Data.Maybe
import Control.Applicative
import Control.Monad.Extra
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.Extra hiding (yield)
import Control.Monad.IO.Class
import General.Str
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
zipFromC !i = awaitJust $ \a -> do
    yield (i,a)
    zipFromC (succ i)

eitherC :: Monad m => ConduitM i1 o m r1 -> ConduitM i2 o m r2 -> ConduitM (Either i1 i2) o m (r1,r2)
eitherC left right = (mapMaybeC l |> left) |$| (mapMaybeC r |> right)
    where l = either Just (const Nothing)
          r = either (const Nothing) Just

rightsC :: Monad m => ConduitM i2 o2 m () -> ConduitM (Either i1 i2) (Either i1 o2) m ()
rightsC c = void $ eitherC (mapC Left) (c |> mapC Right)

countC :: (Monad m, Num c) => Consumer a m c
countC = sumC <| mapC (const 1)

sumC :: (Monad m, Num a) => Consumer a m a
sumC = foldC (+) 0

(|$|) :: Monad m => ConduitM i o m r1 -> ConduitM i o m r2 -> ConduitM i o m (r1,r2)
(|$|) a b = getZipConduit $ (,) <$> ZipConduit a <*> ZipConduit b

sinkList :: Monad m => Consumer a m [a]
sinkList = consume


linesC :: Monad m => Conduit Str m Str
linesC = loop []
    where
        loop acc = await >>= maybe (finish acc) (go acc)

        finish acc = unless (BS.null final) (yield final)
            where final = BS.concat $ reverse acc

        go acc more = case BS.uncons second of
            Just (_, second') -> yield (BS.concat $ reverse $ first:acc) >> go [] second'
            Nothing -> loop $ more:acc
            where (first, second) = BS.break (== '\n') more

linesCR :: Monad m => Conduit Str m Str
linesCR = linesC |> mapC f
    where f x | Just (x, '\r') <- bsUnsnoc x = x
              | otherwise = x

bsUnsnoc :: BS.ByteString -> Maybe (BS.ByteString, Char)
#if __GLASGOW_HASKELL__ < 708
bsUnsnoc x | BS.null x = Nothing
           | otherwise = Just (BS.init x, BS.last x)
#else
bsUnsnoc = BS.unsnoc
#endif

sourceLStr :: Monad m => LStr -> Producer m Str
sourceLStr = sourceList . lstrToChunks


pipelineC :: Int -> Consumer o IO r -> Consumer o IO r
pipelineC buffer sink = do
    sem <- liftIO $ newQSem buffer
    chan <- liftIO newChan
    bar <- liftIO newBarrier
    liftIO $ flip forkFinally (signalBarrier bar) $ do
        runConduit $
            (whileM $ do
                x <- liftIO $ readChan chan
                liftIO $ signalQSem sem
                case x of
                    Nothing -> return False
                    Just x -> do yield x; return True) |>
            sink
    whileM $ do
        signaled <- liftIO $ isJust <$> waitBarrierMaybe bar
        if signaled then
            return False
         else do
            x <- await
            liftIO $ writeChan chan x
            liftIO $ waitQSem sem
            return $ isJust x
    liftIO $ either throwIO return =<< waitBarrier bar
