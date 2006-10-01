
module General.Type where


data TagStr = Str String
            | Tags [TagStr]
            | TagBold TagStr
            | TagUnderline TagStr
            | TagHyperlink String TagStr
            | TagColor Int TagStr


data Response = Warn String
              | Error String


instance Show Response where
    showList = showString . unlines . map show
    show (Warn x) =  "Warning: " ++ x
    show (Error x) = "Error:   " ++ x


isError (Error _) = True; isError _ = False


anyError :: [Response] -> Bool
anyError = any isError
