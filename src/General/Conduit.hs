{-# LANGUAGE NoMonomorphismRestriction, PatternGuards, CPP #-}

module General.Conduit(
    module Data.Conduit, MonadIO, liftIO,
    sourceList, sinkList, sourceLStr,
    mapC, mapAccumC, filterC,
    mapMC, mapAccumMC,
    (|$|), pipelineC, groupOnLastC,
    zipFromC, linesCR
    ) where

import Data.Conduit
import Data.Conduit.List as C
import Data.Conduit.Binary as C
import Data.Maybe
import Control.Applicative
import Control.Monad.Extra
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.Extra hiding (yield)
import Control.Monad.IO.Class
import General.Str
import Data.Void
import Prelude


mapC = C.map
mapMC = C.mapM
mapAccumC f = C.mapAccum (\x a -> a `seq` f a x)
mapAccumMC f = C.mapAccumM (\x a -> a `seq` f a x)
filterC = C.filter

zipFromC :: (Monad m, Enum i) => i -> ConduitM a (i, a) m ()
zipFromC = void . mapAccumC (\i x -> (succ i, (i,x)))

(|$|) :: Monad m => ConduitM i o m r1 -> ConduitM i o m r2 -> ConduitM i o m (r1,r2)
(|$|) a b = getZipConduit $ (,) <$> ZipConduit a <*> ZipConduit b

sinkList :: Monad m => ConduitM a o m [a]
sinkList = consume

-- | Group things while they have the same function result, only return the last value.
--   Conduit version of @groupOnLast f = map last . groupOn f@.
groupOnLastC :: (Monad m, Eq b) => (a -> b) -> ConduitM a a m ()
groupOnLastC op = do
    x <- await
    whenJust x $ \x -> f (op x) x
    where
        f k v = await >>= \x -> case x of
            Nothing -> yield v
            Just v2 | let k2 = op v2 -> do
                when (k /= k2) $ yield v
                f k2 v2


linesCR :: Monad m => ConduitM Str Str m ()
linesCR = C.lines .| mapC f
    where f x | Just (x, '\r') <- BS.unsnoc x = x
              | otherwise = x

sourceLStr :: Monad m => LStr -> ConduitM i Str m ()
sourceLStr = sourceList . lstrToChunks


pipelineC :: Int -> ConduitM o Void IO r -> ConduitM o Void IO r
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
                return $ isJust x) .|
            sink
    awaitForever $ \x -> liftIO $ do
        waitQSem sem
        writeChan chan $ Just x
    liftIO $ writeChan chan Nothing
    liftIO $ waitBarrier bar
