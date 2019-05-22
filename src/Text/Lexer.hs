module Text.Lexer (
    parseLexer
) where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Lexer.DFA
import Text.Lexer.DFA.Regex
import Text.Lexer.Regex
import Control.Monad

type Action = String

data Lexer = Lexer {
    userDefs :: String,
    regexDef :: M.Map String Regex,  -- RE name to Regex
    regexAct :: M.Map String Action, -- RE literal to Action
    regexDFA :: DFA,
    userCode :: String
} deriving (Show)

parseLexer :: GenParser Char Lexer Lexer
parseLexer = do
    skipMany trim
    -- Parse Definition Section
    userDefs <- betweenStrMany "%{" "%}" anyChar
    updateState $ \lexer -> lexer { userDefs }
    -- Parse Regex Definitions
    manyTill (try trim <|> parseRegexDef) (string "%%")
    -- Parse Regex & Actions
    manyTill (try trim <|> parseRegexAct) (string "%%")
    -- Parse User Code
    userCode <- manyTill anyChar eof
    updateState $ \lexer -> lexer { userCode }
    -- Return final state
    getState

parseRegexDef :: GenParser Char Lexer ()
parseRegexDef = do
    Lexer { regexDef } <- getState
    name  <- manyTill anyChar space
    _     <- many trim
    regex <- parseRegex regexDef
    updateState $ \lexer -> lexer {
        regexDef = M.insert name regex regexDef
    }

parseRegexAct :: GenParser Char Lexer ()
parseRegexAct = do
    Lexer { regexDef } <- getState
    literal <- lookAhead $ manyTill anyChar space
    regex   <- parseRegex regexDef
    _       <- many trim
    action  <- manyTill anyChar endOfLine
    updateState $ \lexer@Lexer { regexAct, regexDFA } -> lexer {
        regexAct = M.insert literal action regexAct,
        regexDFA = regex2dfa literal regex <> regexDFA -- Use literal as tag
    }

trim :: GenParser Char Lexer ()
trim = try (void space) <|> void comment -- Trim spaces and comments

comment :: GenParser Char Lexer String
comment = betweenStrMany "/*" "*/" anyChar

lexFileInput = "  \n\t\n%{\n\n#include <stdio.h>\naaa\n%}\nD\t[0-9]\nA\t[1-3]*\n%%\nauto\t{ printf(\"AUTO\"); }\ncase\t{ printf(\"CASE\"); }\n{D}\t{ printf(\"IDENTIFIER\"); }\n%%\nmain() {\tyylex();\n}"

runLexer = case runParser parseLexer (Lexer "" M.empty M.empty empty "") "" lexFileInput of
    Left err -> error $ show err
    Right lexer -> lexer
