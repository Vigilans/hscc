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
    userDefs <- betweenStr "%{" "%}" anyChar
    updateState $ \lexer -> lexer { userDefs }
    -- Parse Regex Definitions
    manyTill (trim <|> parseRegexDef) (string "%%")
    -- Parse Regex & Actions
    manyTill (trim <|> parseRegexAct) (string "%%")
    -- Parse User Code
    userCode <- manyTill anyChar eof
    updateState $ \lexer -> lexer { userCode }
    -- Return final state
    getState

parseRegexDef :: GenParser Char Lexer ()
parseRegexDef = do
    Lexer { regexDef } <- getState
    name  <- manyTill anyChar space
    _     <- trim
    regex <- parseRegex regexDef
    updateState $ \lexer -> lexer {
        regexDef = M.insert name regex regexDef
    }

parseRegexAct :: GenParser Char Lexer ()
parseRegexAct = do
    Lexer { regexDef } <- getState
    literal <- lookAhead $ manyTill anyChar space
    regex   <- parseRegex regexDef
    _       <- trim
    action  <- manyTill anyChar space
    updateState $ \lexer@Lexer { regexAct, regexDFA } -> lexer {
        regexAct = M.insert literal action regexAct,
        regexDFA = union regexDFA $ regex2dfa regex literal -- Use literal as tag
    }

trim :: GenParser Char Lexer ()
trim = void space <|> void comment -- Trim spaces and comments

comment :: GenParser Char Lexer String
comment = betweenStr "/*" "*/" anyChar

betweenStr :: String -> String -> GenParser Char st a -> GenParser Char st [a]
betweenStr open close p = string open >> manyTill p (string close)

input = "a|b|c"

lexFileInput = "  \n\t\n%{\n\n#include <stdio.h>\naaa\n%}\nD\t[0-9]\nA\t[1-3]*\n%%\nauto\t{ printf(\"AUTO\"); }\ncase\t{ printf(\"CASE\"); }\n{D}\t{ printf(\"IDENTIFIER\"); }\n%%\nmain() {\tyylex();\n}"

stripL :: String -> String -> String
stripL x = dropWhile (`elem` x)

stripR :: String -> String -> String
stripR x = reverse . stripL x . reverse

strip :: String -> String -> String
strip x = stripL x . stripR x

runLexer = case runParser parseLexer (Lexer {}) "" lexFileInput of
    Left err -> error $ show err
    Right lexer -> lexer
