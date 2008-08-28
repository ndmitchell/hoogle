
module Hoogle.Lexer.All where


data Lexeme = SingleComment String -- ^ -- foo
            | HaddockComment String -- ^ -- | foo
            | MultiComment String -- ^ {- foo -}
            | Space String -- ^ whitespace
            | Argument ArgType String String -- ^ /foo=bar --foo=bar
            | Operator String -- ^ && -> ,
            | Name String -- ^ foo () [] (#,#)
            | PlusName String -- ^ +foo
            | MinusName String -- ^ -foo
            | BracketOpen String -- ^ ( (# [ [:
            | BracketShut String -- ^ ) #) ] :]

data ArgType = Slash | MinusMinus



bracketsOpen = ["(","[","(#","[:"]
bracketsShut = [")","]","#)",":]"]


-- | On failure return the lexemes that worked up until this point, followed by the rest of the string
lexer :: String -> Either ([Lexeme],String) [Lexeme]
lexer xs = undefined -- should split around each space


-- | Similar to lexer, but don't split up spaces in arguments between lexemes
lexerArgs :: [String] -> Either ([Lexeme],String) [Lexeme]
lexerArgs = undefined


-- | Convert a lexeme to a string
renderLexeme :: Lexeme -> String
renderLexeme = undefined
