{-# LANGUAGE RecordWildCards, ViewPatterns, TupleSections, PatternGuards #-}

module General.Log(
    Log, logCreate, logNone, logAddMessage, logAddEntry,
    Summary(..), logSummary,
    ) where

import Control.Concurrent.Extra
import Control.Applicative
import System.IO
import Data.Time.Calendar
import Data.Time.Clock
import Numeric.Extra
import Control.Monad.Extra
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Monoid
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

logCreate :: Either Handle FilePath -> (String -> Bool) -> IO Log
logCreate store interesting = do
    (h, old) <- case store of
        Left h -> return (h, Map.empty)
        Right file -> do
            mp <- withFile file ReadMode $ \h -> do
                src <- LBS.hGetContents h
                let xs = mapMaybe (parseLogLine interesting) $ LBS.lines $ src
                return $! foldl' (\mp (k,v) -> Map.alter (Just . maybe v (<> v)) k mp) Map.empty xs
            (,mp) <$> openFile file AppendMode
    hSetBuffering h LineBuffering
    var <- newVar h
    ref <- newIORef old
    return $ Log (Just var) ref interesting

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
        add $ SummaryI (Set.singleton user) 1 taken (toAverage taken) (if isJust err then 1 else 0)
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

summarize :: Day -> SummaryI -> Summary
summarize date SummaryI{..} = Summary date (Set.size iUsers) iUses iSlowest (fromAverage iAverage) iErrors

parseLogLine :: (String -> Bool) -> LBS.ByteString -> Maybe (Day, SummaryI)
parseLogLine interesting (LBS.words -> time:user:dur:query:err)
    | user /= LBS.pack "-"
    , Just [a, b, c] <- fmap (map fst) $ mapM LBS.readInt $ LBS.split '-' $ LBS.takeWhile (/= 'T') time
    = Just (fromGregorian (fromIntegral a) b c, SummaryI
        (if use then Set.singleton $ LBS.unpack user else Set.empty)
        (if use then 1 else 0)
        (if use then dur2 else 0)
        (toAverage $ if use then dur2 else 0)
        (if [LBS.pack "ERROR:"] `isPrefixOf` err then 1 else 0))
    where use = interesting $ LBS.unpack query
          dur2 = let s = LBS.unpack dur in fromMaybe 0 $
                 if '.' `elem` s then readMaybe s else (/ 1000) . intToDouble <$> readMaybe s
parseLogLine _ _ = Nothing


logSummary :: Log -> IO [Summary]
logSummary Log{..} = map (uncurry summarize) . Map.toAscList <$> readIORef logCurrent
