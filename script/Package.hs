
import Text.HTML.TagSoup
import Data.List
import Data.Char
import System.Environment
import System.IO.Extra
import Numeric


main :: IO ()
main = do
    [input,output] <- getArgs
    writeFileBinary output . translateKeywords =<< readFile' input

    xs <- listFiles input
    xs <- liftIO $ forM (Map.toList cabs) $ \(name,ver) -> do
        src <- try $ readCabal $ srcCabal name ver
        return $ case src of
            Left (_ :: SomeException) -> []
            Right src ->
                [""] ++ zipWith (++) ("-- | " : repeat "--   ") (cabalDescription src) ++
                ["--","-- Version " ++ ver, "@url package/" ++ name, "@entry package " ++ name]
    liftIO $ writeFileUtf8 output $ unlines $ ("@url " ++ hackage) : "@package package" : concat xs
