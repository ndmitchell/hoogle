{-# LANGUAGE NoMonomorphismRestriction, PatternGuards, Rank2Types, CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- QSem was deprecated in 7.6, but then undeprecated

module General.Conduit(
    module Data.Conduit, MonadIO, liftIO,
    sourceList, sinkList, sourceLStr,
    foldC, mapC, mapMaybeC, mapAccumC, filterC, concatC,
    mapAccumMC,
    (|$|), pipelineC,
    zipFromC, countC, sumC, linesC, linesCR
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


concatC = C.concat
mapC = C.map
foldC = C.fold
mapMaybeC = C.mapMaybe
mapAccumC f = C.mapAccum (flip f)
mapAccumMC f = C.mapAccumM (flip f)
filterC = C.filter

zipFromC :: (Monad m, Enum i) => i -> Conduit a m (i, a)
zipFromC = void . mapAccumC (\i x -> (succ i, (i,x)))

countC :: (Monad m, Num c) => Consumer a m c
countC = mapC (const 1) =$= sumC

sumC :: (Monad m, Num a) => Consumer a m a
sumC = foldC (+) 0

(|$|) :: Monad m => ConduitM i o m r1 -> ConduitM i o m r2 -> ConduitM i o m (r1,r2)
(|$|) a b = getZipConduit $ (,) <$> ZipConduit a <*> ZipConduit b

sinkList :: Monad m => Consumer a m [a]
sinkList = consume


-- | I use this version as in older versions of Conduit the equivalent is O(n^2).
--   https://github.com/snoyberg/conduit/pull/209
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
linesCR = linesC =$= mapC f
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
    sem <- liftIO $ newQSem buffer  -- how many are in flow, to avoid memory leaks
    chan <- liftIO newChan          -- the items in flow (type o)
    bar <- liftIO newBarrier        -- the result type (type r)
    me <- liftIO myThreadId
    liftIO $ flip forkFinally (either (throwTo me) (signalBarrier bar)) $ do
        runConduit $
            (whileM $ do
                x <- liftIO $ readChan chan
                liftIO $ signalQSem sem
                whenJust x yield
                return $ isJust x) =$=
            sink
    awaitForever $ \x -> liftIO $ do
        waitQSem sem
        writeChan chan $ Just x
    liftIO $ writeChan chan Nothing
    liftIO $ waitBarrier bar
