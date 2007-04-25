
-- [] is success
-- (_:_) are the error messages
createDataBase :: TextBase -> FilePath -> IO [Response]
createDataBase tb file = do
    hndl <- openBinaryFile file WriteMode
    hPutStr hndl hooString
    hPutInt hndl 0 -- 0 for binary notice
    hPutInt hndl hooVersion -- verson number

    (as,tb) <- return $ partition (isItemAttribute . itemRest) tb
    let attribs = [(a,b) | ItemAttribute a b <- map itemRest as]

    hPutString hndl "package"
    hPutString hndl (fromMaybe "" $ lookup "web" attribs)

    tablePos <- hGetPosn hndl
    replicateM_ 6 $ hPutInt hndl 0

    posModule <- hGetPos hndl
    tb2 <- saveModules hndl tb
    tb3 <- saveItems hndl tb2 (lookup "local" attribs)
    
    (pos, err) <-
        mapAndUnzipM (\x -> do y <- hGetPos hndl ; z <- x ; return (y,z))
            [saveKinds hndl tb
            ,saveAlias hndl tb
            ,saveInstances hndl tb
            ,saveTexts hndl tb3
            ,saveTypes hndl tb3
            ]
    
    hSetPosn tablePos
    mapM_ (hPutInt hndl) (posModule:pos)
    hClose hndl
    
    return $ concat err


loadDataBase :: FilePath -> IO (Maybe DataBase)
loadDataBase file = do
    hndl <- openBinaryFile file ReadMode
    str <- hGetStr hndl (length hooString)
    zero <- hGetInt hndl
    ver <- hGetInt hndl
    
    if str /= hooString || zero /= 0 || ver /= hooVersion then return Nothing else do
        
        package <- hGetString hndl
        version <- hGetString hndl
        
        [a,b,c,d,e,f] <- replicateM 6 $ hGetInt hndl
        a2 <- gen a; b2 <- gen b; c2 <- gen c; d2 <- gen d
        
        return $ Just $ DataBase hndl package version a2 b2 c2 d2 e f
    where
        gen :: Int -> IO (IORef (Either Int a))
        gen i = newIORef (Left i)
