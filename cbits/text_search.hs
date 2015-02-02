

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import Control.Monad
import Control.Applicative
import System.IO


foreign import ccall text_search_bound :: CString -> IO CInt

foreign import ccall text_search :: CString -> Ptr CString -> CInt -> Ptr CInt -> IO CInt

withs :: Monad m => [(a -> m r) -> m r] -> ([a] -> m r) -> m r
withs [] act = act []
withs (f:fs) act = f $ \a -> withs fs $ \as -> act $ a:as

text_search_bound_ :: String -> IO Int
text_search_bound_ s = withCString s $ fmap fromIntegral . text_search_bound

text_search_ :: String -> [String] -> Bool -> IO [Int]
text_search_ haystack needles exact =
    let n = length $ filter (== '\0') haystack in
    withCString haystack $ \haystack ->
    withs (map withCString needles) $ \needles -> withArray0 nullPtr needles $ \needles ->
    withArray (replicate n 0) $ \result -> do
        found <- text_search haystack needles (if exact then 1 else 0) result
        map fromIntegral <$> peekArray (fromIntegral found) result


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    res <- text_search_bound_ "Test\0abcccccccc\0new\0"
    when (res /= 2) $ error $ show ("text_search_bound", res)
    putChar '.'

    res <- text_search_ " test\0more\0tex\0xtee\0" ["te"] False
    when (res /= [2,0,3]) $ error $ show ("text_search", res)
    putChar '.'

    res <- text_search_ " base\0base\0" ["base"] False
    when (res /= [1,0]) $ error $ show ("text_search", res)
    putChar '.'

    putStrLn ""
