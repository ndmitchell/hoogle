{-# LANGUAGE PatternGuards, RecordWildCards, ScopedTypeVariables #-}

-- | Analyse the log files. It's a three stage process.
--   1, parse each line separately.
--   2, collapse searches done between lines (instant, ajax, suggest)
--   3, draw overall statistics
module Console.Log(logFiles) where

import General.Base
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map

logFiles :: [FilePath] -> IO ()
logFiles xs = do
    es <- mapM readEntries xs
    print $ mconcat $ map (stats . groupEntries) es

---------------------------------------------------------------------
-- STATISTICS

data Stats = Stats
    {hits :: !Int
    ,searches :: !Int
    ,common :: !(Map.Map LBString Int)
    }

instance Show Stats where
    show Stats{..} = unlines
        ["Hits:      " ++ show hits
        ,"Searches:  " ++ show searches
        ,"Unique:    " ++ show (Map.size common)
        ,"Top:       " ++ fromList "" (map (LBS.unpack . fst) top)
        ]
        where
            top = take 20 $ sortBy (comparing $ negate . snd) $ Map.toList common


instance Monoid Stats where
    mempty = Stats 0 0 Map.empty
    mappend (Stats x1 x2 x3) (Stats y1 y2 y3) = Stats (x1+y1) (x2+y2) (Map.unionWith (+) x3 y3)

stats :: [Entry] -> Stats
stats = foldl' f mempty
    where
        f s@Stats{..} Entry{..} = s
            {hits = 1 + hits
            ,searches = (if LBS.null search then 0 else 1) + searches
            ,common = if LBS.null search then common else Map.insertWith' (+) search 1 common
            }


---------------------------------------------------------------------
-- GROUP ENTRIES

groupEntries :: [Entry] -> [Entry]
groupEntries = id


---------------------------------------------------------------------
-- READ ENTRIES

data Entry = Entry
    {search :: LBString -- the search performed, "" for blank
    ,extra :: [(LBString,LBString)] -- extra parameters
    ,date :: Maybe (Int,Int,Int) -- date the search was performed
    ,time :: Maybe (Int,Int,Int) -- time the search was performed
    ,unique :: Maybe String -- maybe a uniquely identifying string
    ,instant :: Maybe Int -- number of times you hit with instant for this query
    ,suggest :: Maybe Int -- number of times you hit with suggest for this query
    } deriving (Eq, Show)

entry = Entry LBS.empty [] Nothing Nothing Nothing Nothing Nothing


readEntries :: FilePath -> IO [Entry]
readEntries x = do
    src <- LBS.readFile x
    return $ mapMaybe readEntry $ LBS.lines src


qstr = map LBS.pack ["","q","hoogle"]

readEntry :: LBString -> Maybe Entry

-- log format v1
readEntry x
    | Just ('[',x) <- LBS.uncons x
    = do y <- readList x
         let (a,b) = partition (flip elem qstr . fst) y
         return entry{search=fromList LBS.empty $ map snd a, extra = b}
    where
        readList x = do
            ('(',x) <- LBS.uncons x
            (a,x) <- readShowString x
            (',',x) <- LBS.uncons x
            (b,x) <- readShowString x
            (')',x) <- LBS.uncons x
            case LBS.uncons x of
                Just (',',x) -> do
                    ys <- readList x
                    return $ (a,b):ys
                Just (']',x) -> do
                    return [(a,b)]
                _ -> Nothing

-- log format v2
readEntry o@x
    | LBS.length x > 10 && LBS.index x 10 == ' '
    = do (d,x) <- readDate x
         (' ',x) <- LBS.uncons x
         (s,x) <- readShowString x
         args <- readArgs $ LBS.dropWhile isSpace x
         return entry{search = s, date = Just d,
                extra = filter (flip notElem qstr . fst) args}
    where
        readArgs x
            | Just ('?',x) <- LBS.uncons x = do
              (a,x) <- return $ LBS.break (== '=') x
              ('=',x) <- LBS.uncons x
              (b,x) <- readQuoteString x
              x <- return $ LBS.dropWhile isSpace x
              ys <- readArgs x
              return $ (a,b) : ys
            | otherwise = Just []
    

-- log format v3
readEntry x
    | LBS.length x > 10 && LBS.index x 10 == 'T'
    = do ((d,t),x) <- readDateTime x
         (' ',x) <- LBS.uncons x
         (u,x) <- return $ first LBS.unpack $ LBS.break (== ' ') x
         args <- readArgs $ LBS.dropWhile isSpace x
         let (a,b) = partition (flip elem qstr . fst) args
         return entry{date = Just d, time = Just t, extra = b,
            search=fromList LBS.empty $ map snd a,
            unique = if u == "0" then Nothing else Just u}
    where
        readArgs x
            | LBS.null x = Just []
            | otherwise = do
                (a,x) <- readShortString x
                ('=',x) <- LBS.uncons x
                (b,x) <- readShortString x
                ys <- readArgs $ LBS.dropWhile isSpace x
                return $ (a,b):ys

readEntry _ = Nothing


---------------------------------------------------------------------
-- READ UTILITIES

readDate :: LBString -> Maybe ((Int,Int,Int), LBString)
readDate x = do
    (d1,x) <- LBS.readInt x
    ('-',x) <- LBS.uncons x
    (d2,x) <- LBS.readInt x
    ('-',x) <- LBS.uncons x
    (d3,x) <- LBS.readInt x
    return ((d1,d2,d2),x)

readDateTime :: LBString -> Maybe (((Int,Int,Int),(Int,Int,Int)), LBString)
readDateTime x = do
    (d,x) <- readDate x
    ('T',x) <- LBS.uncons x
    (t1,x) <- LBS.readInt x
    (':',x) <- LBS.uncons x
    (t2,x) <- LBS.readInt x
    (':',x) <- LBS.uncons x
    (t3,x) <- LBS.readInt x
    return ((d,(t1,t2,t3)),x)


-- | String, as produced by show
readShowString :: LBString -> Maybe (LBString, LBString)
readShowString o@x = do
    ('\"',x) <- LBS.uncons x
    (a,x) <- return $ LBS.break (== '\"') x
    if '\\' `LBS.elem` a then do
        [(a,x)] <- return $ reads $ LBS.unpack o
        return (LBS.pack a, LBS.pack x)
     else do
        ('\"',x) <- LBS.uncons x
        return (a, x)


-- | Either a string produced by show, or a isAlphaNum terminated chunk
readShortString :: LBString -> Maybe (LBString, LBString)
readShortString x | Just ('\"',_) <- LBS.uncons x = readShowString x
                  | otherwise = Just $ LBS.span isAlphaNum x


-- | Either a space terminated chunk, or a quote terminated chunk
readQuoteString :: LBString -> Maybe (LBString, LBString)
readQuoteString x | Just ('\"',x) <- LBS.uncons x = do
    (a,x) <- return $ LBS.break (== '\"') x
    ('\"',x) <- LBS.uncons x
    return (a, LBS.dropWhile isSpace x)
readQuoteString x = do
    (a,x) <- return $ LBS.break (== ' ') x
    return (a, LBS.dropWhile isSpace x)

