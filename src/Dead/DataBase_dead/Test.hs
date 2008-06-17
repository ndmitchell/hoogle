
import Hoogle.DataBase.Type
import Hoogle.TextBase.All


main :: IO ()
main = do x <- parseTextBase "small.txt"
          case x of
            Left x -> error $ show x
            Right x -> do
                r <- createDataBase x "small.hoo"
                error $ show r


main2 :: IO ()
main2 = do x <- loadDataBase "small.hoo"
           case x of
               Nothing -> error "bad database"
               Just x -> do print (version x, package x)
                            res <- searchName x "lete"
                            print res
