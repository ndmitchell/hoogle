
module Main(main) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Environment
import Data.List
import Control.Exception
import Control.Monad


type Callback = CInt -> Ptr CInt -> Ptr CString -> CInt -> IO CInt

foreign import ccall "wrapper" mkCallback :: Callback -> IO (FunPtr Callback)

foreign import ccall "text_search" text_search :: CString -> CInt -> Ptr CString -> FunPtr Callback -> IO ()


output :: Callback
output n path names quality = do
    let ptrs p = mapM (peekElemOff p) [0..fromIntegral n-1]
    path <- ptrs path
    names <- mapM peekCString =<< ptrs names
    putStrLn $ intercalate "." (map show path) ++ " " ++ intercalate "." names ++ " with quality " ++ show quality
    return 0

main :: IO ()
main = do
    args <- getArgs
    bracket (mkCallback output) freeHaskellFunPtr $ \output ->
        withCStrings args $ \cs -> withArrayLen cs $ \n cs ->
            withCString "default.str" $ \file ->
                text_search file (fromIntegral n) cs output

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings [] act = act []
withCStrings (x:xs) act = withCString x $ \c -> withCStrings xs $ act . (c:)

