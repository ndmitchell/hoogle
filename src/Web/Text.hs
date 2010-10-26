
-- | FIXME: Most of this module should be moved elsewhere
module Web.Text where

import General.Code
import Numeric


-- | Only append strings if neither one is empty
(+?) :: String -> String -> String
a +? b = if null a || null b then [] else a ++ b

-- | Escape the second argument as HTML before appending
(+&) :: String -> String -> String
a +& b = a ++ escapeHTML b

-- | Escape the second argument as a CGI query string before appending
(+%) :: String -> String -> String
a +% b = a ++ escapeCGI b


-- TODO: Should be somewhere else
escapeHTML = concatMap f
    where
        f '\"' = "&quot;"
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f '\n' = "<br/>"
        f x = [x]

escapeCGI = concatMap f
    where
        f x | isAlphaNum x || x `elem` "-" = [x]
            | x == ' ' = "+"
            | otherwise = '%' : ['0'|length s == 1] ++ s
            where s = showHex (ord x) ""
