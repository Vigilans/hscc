module Text.Lexer (
    parseLexer
) where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.Lexer.Regex
import Debug.Trace
import Data.List
import qualified Text.Regex.Posix as R
type Action = String

data Lexer = Lexer {
    definitionSection :: String,
    regexDefinitions  :: M.Map String Regex,  -- Tag to Regex
    regexActions      :: M.Map String Action, -- Tag to Action
    userSubroutines   :: String
} deriving (Show)

data LexFile = LexFile {
    headSection         :: String,
    regularDefininions  :: M.Map String String, -- def to RE
    regularExpressions  :: M.Map String String, -- RE to Action
    tailSection         :: String
} deriving (Show)

parseLexer :: GenParser Char Lexer Lexer
parseLexer = do
    r <- parseRegex
    updateState $ \l@Lexer { regexDefinitions } -> l {
        regexDefinitions = M.insert "Hello, World" r regexDefinitions
    }
    getState

input = "a|b|c"

lexFileInput = "  \n\t\n%{\n\n#include <stdio.h>\naaa\n%}\nD\t[0-9]\nA\t[1-3]*\n%%\nauto\t{ printf(\"AUTO\"); }\ncase\t{ printf(\"CASE\"); }\n{D}\t{ printf(\"IDENTIFIER\"); }\n%%\nmain() {\tyylex();\n}"

lexFile :: GenParser Char st [String]
lexFile = do
    many (oneOf "\n\t\f\v ")
    string "%{\n"
    headSec <- manyTill anyChar (try (string "\n%}\n"))
    
    rdSec <- manyTill anyChar (try (string "\n%%\n"))
    reSec <- manyTill anyChar (try (string "\n%%\n"))
    usrSec <- many anyChar
    return [headSec, rdSec, reSec, usrSec]


-- buildRD :: String -> M.Map String String
-- buildRD s = foldl handle M.empty (lines s) where
--     handle acc l = M.insert def pat acc where
--         -- def = head (head ((strip l) R.=~"[^\t\f\v ]*"::[[String]]))
--         def = "bb"
--         pat = "aa"




parseLex :: String -> Either ParseError [String]
parseLex input = parse lexFile "err" input

stripL :: String -> String -> String
stripL x = dropWhile (`elem` x)

stripR :: String -> String -> String
stripR x = reverse . stripL x . reverse

strip :: String -> String -> String
strip x = stripL x . stripR x
run = case runParser parseLexer (Lexer "blabla" M.empty M.empty "lablab") "sdfas" input of
    Left err -> error $ show err
    Right lexer -> lexer
