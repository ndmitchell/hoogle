
module General.UTF8(
    UTF8, utf8Pack, utf8ReadFile, utf8SplitInfix,
    LUTF8, lutf8ToChunks, lutf8FromChunks
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as LBS


type UTF8 = BS.ByteString

type LUTF8 = LBS.ByteString


utf8Pack :: String -> UTF8
utf8Pack = US.fromString

utf8ReadFile :: FilePath -> IO UTF8
utf8ReadFile = BS.readFile

utf8SplitInfix :: UTF8 -> UTF8 -> Maybe (UTF8, UTF8)
utf8SplitInfix needle haystack
    | (a,b) <- BS.breakSubstring needle haystack
    , not $ BS.null b
    = Just (a, BS.drop (BS.length needle) b)
utf8SplitInfix _ _ = Nothing


lutf8ToChunks :: LUTF8 -> [UTF8]
lutf8ToChunks = LBS.toChunks

lutf8FromChunks :: [UTF8] -> LUTF8
lutf8FromChunks = LBS.fromChunks
