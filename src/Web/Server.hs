
module Web.Server(server) where

import General.Code
import Control.Concurrent
import Control.Exception
import Network


server :: IO ()
server = withSocketsDo $ do
    let port = 4000
    sock <- listenOn $ PortNumber port
    bracket
        (forkIO $ forever $ do
            (h,_,_) <- accept sock
            forkIO $ do
                s <- getRequest h
                let url = words (head s) !! 1
                print url
                res <- talk url
                hPutStr h $ intercalate "\r\n" $ "HTTP/1.1 200 OK" : ["",res]
                hClose h
        )
        (\t -> do killThread t ; putStrLn "killThread")
        (const $ do
            putStrLn $ "Started Hoogle Server on port " ++ show port
            _ <- getChar
            return ())


---------------------------------------------------------------------
-- PAGE HANDLER

talk :: String -> IO String
talk x = return $ show x


---------------------------------------------------------------------
-- HTTP METHODS

getRequest :: Handle -> IO [String]
getRequest h = do
    x <- hGetLine h
    if all isSpace x then return [] else do
        xs <- getRequest h
        return $ x : xs
