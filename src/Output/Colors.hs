{-# LANGUAGE MultiWayIf #-}

module Output.Colors (printColored) where

import Data.Char (isLower, isUpper)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Text (pack)
import Data.Void (Void)
import Rainbow
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

type Parser = Parsec Void String
type ColoredString = Chunk

word :: Parser String
word = many (noneOf " [")

line :: Parser String
line = many (anySingleBut '\n')

mergeL :: [Parser [a]] -> Parser [a]
mergeL = fmap concat . sequence

toL :: Parser a -> Parser [a]
toL = fmap (:[])

merge :: [Parser a] -> Parser [a]
merge = mergeL . map toL

ws :: Parser String
ws = many (char ' ')

putColoredLns :: [ColoredString] -> IO ()
putColoredLns = putChunks

data HoogleToken =
      Type      String
    | TypeVar   String
    | Symbols   String
    | Function  String
    | Package   String
    | Keyword   String
    | Unknown   String
    | Text      String
    | Newline
    deriving (Eq, Show)

addNewLine :: Parser [HoogleToken]
addNewLine = pure $ return Newline

signatureParser :: Parser [HoogleToken]
signatureParser = concat <$> manyTill tokenParser' (char '\n')
    where
        tokenParser' :: Parser [HoogleToken]
        tokenParser' = do
            let seps = ",() -=>[]"
            x <- many $ noneOf ('\n':seps)
            y <- fmap Symbols $ many $ oneOf seps
            let x' = if
                    | x == ""          -> Symbols   x
                    | x == "family"    -> Keyword   x
                    | x == "forall"    -> Keyword   x
                    | isUpper $ head x -> Type      x
                    | isLower $ head x -> TypeVar   x
                    | otherwise        -> Symbols   x
            return [x', y]

newtypeParser :: Parser [HoogleToken]
newtypeParser = mergeL
    [ merge
        [ Package  <$> word
        , Symbols  <$> ws
        , Keyword  <$> string "newtype"
        ]
    , signatureParser
    , addNewLine
    ]

dataParser :: Parser [HoogleToken]
dataParser = mergeL
    [ merge
        [ Package  <$> word
        , Symbols  <$> ws
        , Keyword  <$> string "data"
        ]
    , signatureParser
    , addNewLine
    ]

functionSignatureParser :: Parser [HoogleToken]
functionSignatureParser = mergeL
    [ merge
        [ Package  <$> word
        , Symbols  <$> ws
        , Function <$> word
        , Symbols  <$> ws
        , Symbols  <$> string "::"
        ]
    , signatureParser
    , addNewLine
    ]

typeAliasParser :: Parser [HoogleToken]
typeAliasParser = mergeL
    [ merge
        [ Package   <$> word
        , Symbols   <$> ws
        , Keyword   <$> string "type"
        , Symbols   <$> ws
        , Type      <$> word
        , Symbols   <$> ws
        , TypeVar <$> many (anySingleBut '=')
        , Symbols   <$> string "="
        ]
    , signatureParser
    , addNewLine
    ]

typeFamilyParser :: Parser [HoogleToken]
typeFamilyParser = mergeL
    [ merge
        [ Package <$> word
        , Symbols <$> ws
        , Keyword <$> string "type family"
        , Symbols <$> ws
        , Type    <$> word
        ]
    , signatureParser
    , addNewLine
    ]

packageParser :: Parser [HoogleToken]
packageParser = merge
    [ Keyword <$> string "package "
    , Package <$> line
    , Newline <$  char '\n'
    ]

classParser :: Parser [HoogleToken]
classParser = mergeL
    [ merge
        [ Package <$> word
        , Symbols <$> ws
        , Keyword <$> string "class"
        ]
    , signatureParser
    , addNewLine
    ]

moduleParser :: Parser [HoogleToken]
moduleParser = merge
    [ Keyword <$> string "module "
    , Package <$> line
    , Newline <$  char '\n'
    ]

unknownParser :: Parser [HoogleToken]
unknownParser = merge
    [ Unknown <$> line
    , Newline <$  newline
    ]

linesParser :: Parser [HoogleToken]
linesParser = concat <$> manyTill lineParser eof

lineParser :: Parser [HoogleToken]
lineParser = choice $ fmap try
    [ moduleParser
    , packageParser
    , typeAliasParser
    , typeFamilyParser
    , classParser
    , dataParser
    , newtypeParser
    , functionSignatureParser
    , unknownParser
    ]

typeToColored :: HoogleToken -> ColoredString
typeToColored (Type             x) = applyColor blue brightBlue x
typeToColored (TypeVar          x) = applyColor red brightRed x
typeToColored (Symbols          x) = applyColor white white x
typeToColored (Function         x) = applyColor green brightGreen x
typeToColored (Package          x) = applyColor green green x
typeToColored (Keyword          x) = applyColor white grey x
typeToColored (Unknown          x) = applyColor white white x
typeToColored (Text             x) = applyColor white white x
typeToColored  Newline             = applyColor white white "\n"

runParsers :: String -> Either (ParseErrorBundle String Void) [HoogleToken]
runParsers = parse linesParser "Hoogle output parsing error"

applyColor :: Radiant -> Radiant -> String -> ColoredString
applyColor c8 c256 s = chunk (pack s) & fore (c8 <> only256 c256)

highlightHoogle :: String -> [ColoredString]
highlightHoogle = map typeToColored . fromRight (error "This should never happend due to `unknown parser`") . runParsers

printColored :: String -> IO ()
printColored = putColoredLns . highlightHoogle
