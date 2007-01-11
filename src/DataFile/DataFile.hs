
module DataFile.DataFile where

import Prelude hiding (catch)
import System.IO
import Foreign
import Control.Monad
import Control.Exception

import Debug.Trace


data Item = Foo Int Bool
          | Blah String
          deriving Show


val = [Foo 1 True, Foo 3 False, Blah "neil ajsklsdafjkl safdkjlfdsajk ladsfjk lafdsjklafsdjkl", Blah "fred", Foo 18 True]

save = do hndl <- openBinaryFile "temp.txt" WriteMode
          dataWrite hndl val
          hClose hndl

load = do hndl <- openBinaryFile "temp.txt" ReadMode
          val <- dataRead hndl
          -- hClose hndl
          print (last val :: Item)


class DataFile a where
    -- take a data value
    -- return a function to save that data value
    -- and one to load it
    dataFile :: (Handle -> a -> IO (), Handle -> IO a)
    dataFile = (dataWrite, dataRead)
    
    dataWrite :: Handle -> a -> IO ()
    dataWrite = fst dataFile
    
    dataRead :: Handle -> IO a
    dataRead = snd dataFile


instance DataFile Item where
    dataFile = serial
        [\ ~(Foo a b) -> unit Foo << a << b
        ,\ ~(Blah a) -> unit Blah <<~ a
        ]


serial :: [a -> (Handle -> Int -> IO (), Handle -> IO a)] -> (Handle -> a -> IO (), Handle -> IO a)
serial xs = (save, load)
    where
        save hndl value = f $ zip [0::Int ..] xs
            where
                f [] = error "unmatched item to save, or trying to save _|_"
                f ((i,x):xs) = catch (fst (x value) hndl i) (const $ f xs)

        load hndl = do
            i <- dataRead hndl
            snd ((xs !! i) undefined) hndl



instance DataFile Int where
    dataWrite hndl x = hPutStr hndl (replicate (10 - length s) ' ' ++ s)
        where s = show x

    dataRead hndl = replicateM 10 (hGetChar2 hndl) >>= return . read


hGetChar2 hndl = putStrLn "1" >> hGetChar hndl

instance DataFile a => DataFile [a] where
    dataWrite hndl xs = dataWrite hndl (length xs) >> mapM_ (dataWrite hndl) xs
    
    dataRead hndl = do i <- dataRead hndl; print i; replicateM i (dataRead hndl)

instance DataFile Char where
    dataWrite hndl x = dataWrite hndl (fromEnum x)
    dataRead hndl = liftM toEnum $ dataRead hndl

instance DataFile Bool where
    dataWrite hndl x = dataWrite hndl (fromEnum x)
    dataRead hndl = liftM toEnum $ dataRead hndl


unit :: a -> (Handle -> Int -> IO (), Handle -> IO a)
unit f = (dataWrite, const $ return f)


(<<) :: DataFile a => (Handle -> Int -> IO (), Handle -> IO (a -> b)) -> a -> (Handle -> Int -> IO (), Handle -> IO b)
sl << x = combine sl x dataFile

(<<~) :: DataFile a => (Handle -> Int -> IO (), Handle -> IO (a -> b)) -> a -> (Handle -> Int -> IO (), Handle -> IO b)
sl <<~ x = combine sl x (lazyWrite, lazyRead)
    where
        (save,load) = dataFile
        
        lazyWrite hndl x = do
            begin <- hGetPos hndl
            dataWrite hndl (0 :: Int)
            save hndl x
            end <- hGetPos hndl
            hSetPos hndl begin
            dataWrite hndl end
            hSetPos hndl end
            
        lazyRead hndl = do
            end <- dataRead hndl
            begin <- hGetPos hndl
            hSetPos hndl end
            return $ unsafePerformIO (hSetPos hndl begin >> load hndl)

        hGetPos :: Handle -> IO Int
        hGetPos = liftM fromInteger . hTell
        
        hSetPos :: Handle -> Int -> IO ()
        hSetPos hndl = hSeek hndl AbsoluteSeek . toInteger

combine :: (Handle -> Int -> IO (), Handle -> IO (a -> b)) -> a -> (Handle -> a -> IO (), Handle -> IO a) -> (Handle -> Int -> IO (), Handle -> IO b)
combine (save,load) x (s,l) = (\hndl i -> x `seq` save hndl i >> s hndl x, \hndl -> do f <- load hndl; x2 <- l hndl; return (f x2))
