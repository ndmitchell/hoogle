
import Hoogle.DataBase.Type
import Hoogle.TextBase.All


main :: IO ()
main = do x <- parseTextBase "small.txt"
          case x of
            Left x -> error $ show x
            Right x -> do
                r <- createDataBase x "small.hoo"
                error $ show r

