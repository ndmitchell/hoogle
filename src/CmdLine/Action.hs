
module CmdLine.Action(cmdParseError) where

import Text.ParserCombinators.Parsec


cmdParseError :: String -> ParseError -> String
cmdParseError orig err = unlines
    ["Parse error:"
    ,orig
    ,replicate (sourceColumn (errorPos err) - 1) ' ' ++ "^"
    ] ++ show err
