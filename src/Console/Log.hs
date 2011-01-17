{-# LANGUAGE PatternGuards, RecordWildCards, ScopedTypeVariables #-}

-- | Analyse the log files. It's a three stage process.
--   1, parse each line separately.
--   2, collapse searches done between lines (instant, ajax, suggest)
--   3, draw overall statistics
module Console.Log(logFiles) where

import General.Base
import qualified Data.ByteString.Lazy.Char8 as BS


logFiles :: [FilePath] -> IO ()
logFiles xs = do
    es <- mapM readEntries xs
    print $ mconcat $ map (stats . groupEntries) es

---------------------------------------------------------------------
-- STATISTICS

data Stats = Stats
    {hits :: !Int
    ,searches :: !Int
    }

instance Show Stats where
    show Stats{..} = unlines
        ["Hits:      " ++ show hits
        ,"Searches:  " ++ show searches
        ]

instance Monoid Stats where
    mempty = Stats 0 0
    mappend (Stats x1 x2) (Stats y1 y2) = Stats (x1+y1) (x2+y2)

stats :: [Entry] -> Stats
stats = foldl' f mempty
    where
        f s@Stats{..} Entry{..} = s{hits = 1 + hits, searches = (if null search then 0 else 1) + searches}


---------------------------------------------------------------------
-- GROUP ENTRIES

groupEntries :: [Entry] -> [Entry]
groupEntries = id


---------------------------------------------------------------------
-- READ ENTRIES

data Entry = Entry
    {search :: String -- the search performed, "" for blank
    ,extra :: [(String,String)] -- extra parameters
    ,date :: Maybe (Int,Int,Int) -- date the search was performed
    ,time :: Maybe (Int,Int,Int) -- time the search was performed
    ,unique :: Maybe String -- maybe a uniquely identifying string
    } deriving Show

entry = Entry "" [] Nothing Nothing Nothing


readEntries :: FilePath -> IO [Entry]
readEntries x = do
    src <- BS.readFile x
    return $ mapMaybe readEntry $ BS.lines src


readEntry :: BS.ByteString -> Maybe Entry

-- log format v1
readEntry x
    | Just ('[',x) <- BS.uncons x
    = do y <- readList x
         let (a,b) = partition (flip elem ["","q"] . fst) y
         return entry{search=fromList "" $ map snd a, extra = b}
    where
        readList x = do
            ('(',x) <- BS.uncons x
            (a,x) <- readShowString x
            (',',x) <- BS.uncons x
            (b,x) <- readShowString x
            (')',x) <- BS.uncons x
            case BS.uncons x of
                Just (',',x) -> do
                    ys <- readList x
                    return $ (a,b):ys
                Just (']',x) -> do
                    return [(a,b)]
                _ -> Nothing

-- log format v2
readEntry o@x
    | BS.length x > 10 && BS.index x 10 == ' '
    = do (d,x) <- readDate x
         (' ',x) <- BS.uncons x
         (s,x) <- readShowString x
         args <- readArgs $ BS.dropWhile isSpace x
         return entry{search = s, date = Just d,
             extra = filter (flip notElem ["","q","hoogle"] . fst) args}
    where
        readArgs x
            | Just ('?',x) <- BS.uncons x = do
              (a,x) <- return $ first BS.unpack $ BS.break (== '=') x
              ('=',x) <- BS.uncons x
              (b,x) <- readQuoteString x
              x <- return $ BS.dropWhile isSpace x
              ys <- readArgs x
              return $ (a,b) : ys
            | otherwise = Just []
    

-- log format v3
readEntry x
    | BS.length x > 10 && BS.index x 10 == 'T'
    = do ((d,t),x) <- readDateTime x
         (' ',x) <- BS.uncons x
         (u,x) <- return $ first BS.unpack $ BS.break (== ' ') x
         args <- readArgs $ BS.dropWhile isSpace x
         let (a,b) = partition (flip elem ["","q","hoogle"] . fst) args
         return entry{date = Just d, time = Just t, extra = b,
            search=fromList "" $ map snd a,
            unique = if u == "0" then Nothing else Just u}
    where
        readArgs x
            | BS.null x = Just []
            | otherwise = do
                (a,x) <- readShortString x
                ('=',x) <- BS.uncons x
                (b,x) <- readShortString x
                ys <- readArgs $ BS.dropWhile isSpace x
                return $ (a,b):ys

readEntry _ = Nothing


---------------------------------------------------------------------
-- READ UTILITIES

readDate :: BS.ByteString -> Maybe ((Int,Int,Int), BS.ByteString)
readDate x = do
    (d1,x) <- BS.readInt x
    ('-',x) <- BS.uncons x
    (d2,x) <- BS.readInt x
    ('-',x) <- BS.uncons x
    (d3,x) <- BS.readInt x
    return ((d1,d2,d2),x)

readDateTime :: BS.ByteString -> Maybe (((Int,Int,Int),(Int,Int,Int)), BS.ByteString)
readDateTime x = do
    (d,x) <- readDate x
    ('T',x) <- BS.uncons x
    (t1,x) <- BS.readInt x
    (':',x) <- BS.uncons x
    (t2,x) <- BS.readInt x
    (':',x) <- BS.uncons x
    (t3,x) <- BS.readInt x
    return ((d,(t1,t2,t3)),x)


-- | String, as produced by show
readShowString :: BS.ByteString -> Maybe (String, BS.ByteString)
readShowString o@x = do
    ('\"',x) <- BS.uncons x
    (a,x) <- return $ BS.break (== '\"') x
    if '\\' `BS.elem` a then do
        [(a,x)] <- return $ reads $ BS.unpack o
        return (a, BS.pack x)
     else do
        ('\"',x) <- BS.uncons x
        return (BS.unpack a, x)


-- | Either a string produced by show, or a isAlphaNum terminated chunk
readShortString :: BS.ByteString -> Maybe (String, BS.ByteString)
readShortString x | Just ('\"',_) <- BS.uncons x = readShowString x
                  | otherwise = Just $ first BS.unpack $ BS.span isAlphaNum x


-- | Either a space terminated chunk, or a quote terminated chunk
readQuoteString :: BS.ByteString -> Maybe (String, BS.ByteString)
readQuoteString x | Just ('\"',x) <- BS.uncons x = do
    (a,x) <- return $ BS.break (== '\"') x
    ('\"',x) <- BS.uncons x
    return (BS.unpack a, BS.dropWhile isSpace x)
readQuoteString x = do
    (a,x) <- return $ BS.break (== ' ') x
    return (BS.unpack a, BS.dropWhile isSpace x)

