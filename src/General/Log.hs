{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module General.Log(
    Log, logCreate, logNone, logAddMessage, logAddEntry,
    Summary(..), logSummary
    ) where

import Control.Concurrent.Extra
import Control.Applicative
import System.IO
import Data.Time.Clock
import Numeric.Extra
import Control.Monad.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Monoid
import General.Util
import Data.Maybe
import Data.List
import System.Info.Extra
import System.IO.Extra
import System.Process.Extra


data Log = Log
    {logLock :: Lock
    ,logHandle :: Maybe Handle
    ,logFile :: Maybe FilePath
    ,logInteresting :: String -> Bool
    }

showTime :: UTCTime -> String
showTime = showUTCTime "%Y-%m-%dT%H:%M:%S%Q"

logNone :: IO Log
logNone = do lock <- newLock; return $ Log lock Nothing Nothing (const False)

logCreate :: Either Handle FilePath -> (String -> Bool) -> IO Log
logCreate store interesting = do
    logHandle <- either return (`openFile` AppendMode) store
    hSetBuffering logHandle LineBuffering
    logLock <- newLock
    return $ Log logLock (Just logHandle) (either (const Nothing) Just store) interesting

logAddMessage :: Log -> String -> IO ()
logAddMessage Log{..} msg = do
    time <- showTime <$> getCurrentTime
    whenJust logHandle $ \h -> do
        withLock logLock $ hPutStrLn h $ time ++ " - " ++ msg
        hFlush h

logAddEntry :: Log -> String -> String -> Double -> Maybe String -> IO ()
logAddEntry Log{..} user question taken err = do
    time <- showTime <$> getCurrentTime
    whenJust logHandle $ \h -> withLock logLock $ hPutStrLn h $ unwords $
        [time, user, showDP 3 taken, question] ++ maybeToList (fmap ("ERROR: " ++) err)

-- Summary collapsed
data Summary = Summary
    {summaryDate :: String
    ,summaryUsers :: Int
    ,summaryUses :: Int
    ,summarySlowest :: Double
    ,summaryAverage :: Double
    ,summaryErrors :: Int
    }

-- Summary accumulating
data SummaryI = SummaryI
    {iUsers :: !(Set.Set String) -- number of distinct users
    ,iUses :: !Int -- number of uses
    ,iSlowest :: !Double -- slowest result
    ,iAverage :: !(Average Double) -- average result
    ,iErrors :: !Int -- number of errors
    }

instance Monoid SummaryI where
    mempty = SummaryI Set.empty 0 0 (toAverage 0) 0
    mappend (SummaryI x1 x2 x3 x4 x5) (SummaryI y1 y2 y3 y4 y5) =
        SummaryI (f x1 y1) (x2+y2) (max x3 y3) (x4 <> y4) (x5+y5)
        -- more efficient union for the very common case of a single element
        where f x y | Set.size x == 1 = Set.insert (head $ Set.toList x) y
                    | Set.size y == 1 = Set.insert (head $ Set.toList y) x
                    | otherwise = Set.union x y

summarize :: String -> SummaryI -> Summary
summarize date SummaryI{..} = Summary date (Set.size iUsers) iUses iSlowest (fromAverage iAverage) iErrors

parseLogLine :: (String -> Bool) -> LBS.ByteString -> Maybe (String, SummaryI)
parseLogLine interesting (LBS.words -> time:user:dur:rest) | user /= LBS.pack "-" = Just
    (LBS.unpack $ LBS.takeWhile (/= 'T') time, SummaryI
        (if use then Set.singleton $ LBS.unpack user else Set.empty)
        (if use then 1 else 0)
        (if use then time2 else 0)
        (toAverage $ if use then time2 else 0)
        (if [LBS.pack "ERROR:"] `isPrefixOf` drop 1 rest then 1 else 0))
    where use = any (interesting . LBS.unpack) $ take 1 rest
          time2 = fromMaybe 0 $ readMaybe $ LBS.unpack time


logSummary :: Log -> IO [Summary]
logSummary Log{..}
    | Just s <- logFile = do
        src <- readLog s
        let xs = mapMaybe (parseLogLine logInteresting) $ LBS.lines $ LBS.fromChunks [src]
        let mp = foldl' (\mp (k,v) -> Map.alter (Just . maybe v (<> v)) k mp) Map.empty xs
        return $ map (uncurry summarize) $ Map.toAscList mp
    | otherwise = return []


readLog :: FilePath -> IO BS.ByteString
readLog file = withTempFile $ \tmp -> do
    systemOutput_ $ (if isWindows then "copy" else "cp") ++ " \"" ++ file ++ "\" \"" ++ tmp ++ "\""
    BS.readFile tmp
