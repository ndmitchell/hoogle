{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module General.Log(
    Log, logCreate, logAddMessage, logAddEntry,
    Summary(..), logSummary
    ) where

import Control.Concurrent.Extra
import Control.Applicative
import System.IO
import Data.Time.Clock
import Numeric.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import General.Util
import Data.Maybe
import Data.List
import System.Info.Extra
import System.IO.Extra
import System.Process.Extra


data Log = Log
    {logLock :: Lock
    ,logHandle :: Handle
    ,logFile :: Maybe FilePath
    ,logInteresting :: String -> Bool
    }

showTime :: UTCTime -> String
showTime = showUTCTime "%Y-%m-%dT%H:%M:%S%Q"

logCreate :: Either Handle FilePath -> (String -> Bool) -> IO Log
logCreate store interesting = do
    logHandle <- either return (`openFile` AppendMode) store
    hSetBuffering logHandle LineBuffering
    logLock <- newLock
    return $ Log logLock logHandle (either (const Nothing) Just store) interesting

logAddMessage :: Log -> String -> IO ()
logAddMessage Log{..} msg = do
    time <- showTime <$> getCurrentTime
    withLock logLock $ hPutStrLn logHandle $ unwords $
        [time, "-", msg]
    hFlush logHandle

logAddEntry :: Log -> String -> String -> Double -> Maybe String -> IO ()
logAddEntry Log{..} user question taken err = do
    time <- showTime <$> getCurrentTime
    withLock logLock $ hPutStrLn logHandle $ unwords $
        [time, user, show $ ceiling $ taken * 1000, question] ++ maybeToList (fmap ("ERROR: " ++) err)

data Summary = Summary
    {summaryDate :: String
    ,summaryUsers :: Int
    ,summaryUses :: Int
    ,summarySlowest :: Double
    ,summaryAverage :: Double
    ,summaryErrors :: Int
    }


logSummary :: Log -> IO [Summary]
logSummary Log{..} | Just s <- logFile = analyseLog <$> readLog s
                   | otherwise = return []


data Average = Average !Int !Int deriving Show -- a / b

average (Average a b) = intToDouble a / intToDouble b

instance Monoid Average where
    mempty = Average 0 0
    mappend (Average x1 x2) (Average y1 y2) = Average (x1+y1) (x2+y2)

data Info = Info
    {searchIPs :: !(Set.Set BS.ByteString) -- number of IP addresses doing a hoogle= search
    ,searchCount :: !Int -- number of searches in total
    ,slowestPage :: !Int -- slowest page load time (/ or hoogle=)
    ,averagePage :: !Average -- time taken to display a page on average
    ,errorCount :: !Int -- number of instances of error
    } deriving Show

instance Monoid Info where
    mempty = Info Set.empty 0 0 mempty 0
    mappend (Info x1 x2 x3 x4 x5) (Info y1 y2 y3 y4 y5) =
        Info (x1 <> y1) (x2 + y2) (x3 + y3) (x4 <> y4) (x5 + y5)

-- | Per day
-- number of IP addresses containing hoogle=
-- number of searches containing hoogle=
-- longest page, average search or home page
-- any instances of ERROR
analyseLog :: BS.ByteString -> [Summary]
analyseLog = view . foldl' add Map.empty . BS.lines
    where
        view mp = map f $ Map.toAscList mp
            where
                f (t, Info{..}) = Summary (BS.unpack t) (Set.size searchIPs) searchCount (intToDouble slowestPage) (average averagePage) errorCount

        add mp (BS.words -> date:ip:(BS.readInt -> Just (time,_)):url)
            | ip /= BS.pack "-" = Map.alter (Just . add2 . fromMaybe mempty) (BS.takeWhile (/= 'T') date) mp
            where
                isPage = isSearch || url == [BS.pack "/"]
                isSearch = any (BS.pack "hoogle=" `BS.isInfixOf`) url
                isError = any (BS.pack "ERROR:" ==) url
                add2 Info{..} = Info
                    (if isSearch then Set.insert ip searchIPs else searchIPs)
                    (searchCount + if isSearch then 1 else 0)
                    (max slowestPage $ if isPage then time else 0)
                    (averagePage <> if isPage then Average time 1 else mempty)
                    (errorCount + if isError then 1 else 0)
        add mp _ = mp


readLog :: FilePath -> IO BS.ByteString
readLog file = withTempFile $ \tmp -> do
    systemOutput_ $ (if isWindows then "copy" else "cp") ++ " \"" ++ file ++ "\" \"" ++ tmp ++ "\""
    BS.readFile tmp
