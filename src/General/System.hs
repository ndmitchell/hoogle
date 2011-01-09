{-# LANGUAGE CPP #-}

-- | Module for system like things in base/directory/etc, or could plausibly be added.
module General.System(module General.System, module X) where

import System.Cmd as X
import System.Directory as X
import System.Environment as X
import System.Exit as X
import System.IO as X
import System.Mem as X (performGC)


import General.Base
import qualified Control.Exception as E

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
#endif


withDirectory dir cmd = E.bracket
    (do x <- getCurrentDirectory; setCurrentDirectory dir; return x)
    setCurrentDirectory
    (const cmd)


-- FIXME: This could use a lot more bracket calls!
captureOutput :: IO () -> IO (Maybe String)
#if __GLASGOW_HASKELL__ < 612
captureOutput act = return Nothing
#else
captureOutput act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hlint"
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    res <- readFile' f
    removeFile f
    return $ Just res
#endif


system_ :: String -> IO ()
system_ x = do
    res <- system x
    when (res /= ExitSuccess) $ error $ "System command failed: " ++ x


exitMessage :: [String] -> IO a
exitMessage msg = putStr (unlines msg) >> exitFailure


readFileUtf8 :: FilePath -> IO String
#if __GLASGOW_HASKELL__ < 612
readFileUtf8 x = readFile x
#else
readFileUtf8 x = do
    h <- openFile x ReadMode
    hSetEncoding h utf8
    hGetContents h
#endif


writeFileUtf8 :: FilePath -> String -> IO ()
#if __GLASGOW_HASKELL__ < 612
writeFileUtf8 x y = writeFile x y
#else
writeFileUtf8 x y = E.bracket (openFile x WriteMode) hClose $ \h -> do
    hSetEncoding h utf8
    hPutStr h y
#endif
