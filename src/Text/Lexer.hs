module Text.Lexer (
    parseLexer
) where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.Lexer.Regex
import Debug.Trace
import Data.List

type Action = String

data Lexer = Lexer {
    definitionSection :: String,
    regexDefinitions  :: M.Map String Regex,  -- Tag to Regex
    regexActions      :: M.Map String Action, -- Tag to Action
    userSubroutines   :: String
} deriving (Show)

parseLexer :: GenParser Char Lexer Lexer
parseLexer = do
    r <- parseRegex
    updateState $ \l@Lexer { regexDefinitions } -> l {
        regexDefinitions = M.insert "Hello, World" r regexDefinitions
    }
    getState

input = "a|b|c"

run = case runParser parseLexer (Lexer "blabla" M.empty M.empty "lablab") "sdfas" input of
    Left err -> error $ show err
    Right lexer -> lexer
