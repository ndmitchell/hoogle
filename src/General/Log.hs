{-# LANGUAGE RecordWildCards, ViewPatterns, TupleSections, PatternGuards #-}

module General.Log(
    Log, logCreate, logNone, logAddMessage, logAddEntry,
    Summary(..), logSummary,
    ) where

import Control.Concurrent.Extra
import Control.Applicative
import System.Directory
import System.IO
import Data.Hashable
import Data.Time.Calendar
import Data.Time.Clock
import Numeric.Extra
import Control.Monad.Extra
import qualified Data.IntSet as Set
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Semigroup
import General.Util
import Data.Maybe
import Data.List
import Data.IORef
import Prelude


data Log = Log
    {logOutput :: Maybe (Var Handle)
    ,logCurrent :: IORef (Map.Map Day SummaryI)
    ,logInteresting :: String -> Bool
    }

showTime :: UTCTime -> String
showTime = showUTCTime "%Y-%m-%dT%H:%M:%S%Q"

logNone :: IO Log
logNone = do ref <- newIORef Map.empty; return $ Log Nothing ref (const False)

logCreate :: Either Handle FilePath -> (BS.ByteString -> Bool) -> IO Log
logCreate store interesting = do
    (h, old) <- case store of
        Left h -> return (h, Map.empty)
        Right file -> do
            b <- doesFileExist file
            mp <- if not b then return Map.empty else withFile file ReadMode $ \h -> do
                src <- LBS.hGetContents h
                let xs = mapMaybe (parseLogLine interesting . LBS.toStrict) $ LBS.lines src
                return $! foldl' (\mp (k,v) -> Map.alter (Just . maybe v (<> v)) k mp) Map.empty xs
            (,mp) <$> openFile file AppendMode
    hSetBuffering h LineBuffering
    var <- newVar h
    ref <- newIORef old
    return $ Log (Just var) ref (interesting . BS.pack)

logAddMessage :: Log -> String -> IO ()
logAddMessage Log{..} msg = do
    time <- showTime <$> getCurrentTime
    whenJust logOutput $ \var -> withVar var $ \h ->
        hPutStrLn h $ time ++ " - " ++ msg

logAddEntry :: Log -> String -> String -> Double -> Maybe String -> IO ()
logAddEntry Log{..} user question taken err = do
    time <- getCurrentTime
    let add v = atomicModifyIORef logCurrent $ \mp -> (Map.alter (Just . maybe v (<> v)) (utctDay time) mp, ())
    if logInteresting question then
        add $ SummaryI (Set.singleton $ hash $ LBS.pack user) 1 taken (toAverage taken) (if isJust err then 1 else 0)
     else if isJust err then
        add mempty{iErrors=1}
     else
        return ()
    whenJust logOutput $ \var -> withVar var $ \h ->
        hPutStrLn h $ unwords $ [showTime time, user, showDP 3 taken, question] ++
                                maybeToList (fmap ((++) "ERROR: " . unwords . words) err)

-- Summary collapsed
data Summary = Summary
    {summaryDate :: Day
    ,summaryUsers :: {-# UNPACK #-} !Int
    ,summaryUses :: {-# UNPACK #-} !Int
    ,summarySlowest :: {-# UNPACK #-} !Double
    ,summaryAverage :: {-# UNPACK #-} !(Average Double)
    ,summaryErrors :: {-# UNPACK #-} !Int
    }

-- Summary accumulating
data SummaryI = SummaryI
    {iUsers :: !Set.IntSet -- number of distinct users
    ,iUses :: !Int -- number of uses
    ,iSlowest :: !Double -- slowest result
    ,iAverage :: !(Average Double) -- average result
    ,iErrors :: !Int -- number of errors
    }

instance Semigroup SummaryI where
    SummaryI x1 x2 x3 x4 x5 <> SummaryI y1 y2 y3 y4 y5 =
        SummaryI (f x1 y1) (x2+y2) (max x3 y3) (x4 <> y4) (x5+y5)
        -- more efficient union for the very common case of a single element
        where f x y | Set.size x == 1 = Set.insert (head $ Set.toList x) y
                    | Set.size y == 1 = Set.insert (head $ Set.toList y) x
                    | otherwise = Set.union x y

instance Monoid SummaryI where
    mempty = SummaryI Set.empty 0 0 (toAverage 0) 0
    mappend = (<>)

summarize :: Day -> SummaryI -> Summary
summarize date SummaryI{..} = Summary date (Set.size iUsers) iUses iSlowest iAverage iErrors

-- This noinline solves a massive memory leak at -O2, and I have no idea why
{-# NOINLINE parseLogLine #-}
parseLogLine :: (BS.ByteString -> Bool) -> BS.ByteString -> Maybe (Day, SummaryI)
parseLogLine interesting (BS.words -> time:user:dur:query:err)
    | use || isErr
    , user /= BS.singleton '-'
    , Just [a, b, c] <- fmap (map fst) $ mapM BS.readInt $ BS.split '-' $ BS.takeWhile (/= 'T') time
    = Just (fromGregorian (fromIntegral a) b c, SummaryI
        (if use then Set.singleton $ hash user else Set.empty)
        (if use then 1 else 0)
        (if use then dur2 else 0)
        (toAverage $ if use then dur2 else 0)
        (if isErr then 1 else 0))
    where use = interesting query
          isErr = [BS.pack "ERROR:"] `isPrefixOf` err
          dur2 = parseDuration dur
parseLogLine _ _ = Nothing

-- Hoogle used to store whole numbers of milliseconds, then it switched to 4dp doubles with a guaranteed '.'
parseDuration :: BS.ByteString -> Double
parseDuration x
    | Just (whole, x) <- BS.readInt x
    = case BS.uncons x of
        Just ('.', x)
            | Just (frac, y) <- BS.readInt x
                -> intToDouble whole + (intToDouble frac / (10 ^ (BS.length x - BS.length y)))
            | otherwise -> 0
        _ -> intToDouble whole / 1000
parseDuration _ = 0


logSummary :: Log -> IO [Summary]
logSummary Log{..} = map (uncurry summarize) . Map.toAscList <$> readIORef logCurrent
