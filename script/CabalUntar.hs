
-- Given an extracted Cabal tarball, grab just the most recent Cabal file for each

import System.Directory.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad
import Data.Function

main = do
    xs <- listFilesRecursive "index"
    print $ length xs
    files <- return $ map (maximumBy (compare `on` f) . snd) $ groupSort [(takeDirectory $ takeDirectory x, x) | x <- xs ]
    print $ length files
    forM_ files $ \x -> do
        copyFile x ("out" </> takeFileName x)

f :: String -> [Int]
f = map read . wordsBy (== '.') . takeFileName . takeDirectory
