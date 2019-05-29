module Text.Lexer.Regex where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set.Monad as S
import Data.Map ((!))
import Data.List ((\\))
import Text.ParserCombinators.Parsec hiding (State)

data Regex = Epsilon
           | Symbol Char
           | Union  Regex Regex
           | Concat Regex Regex
           | Closure Regex
           | Endmark
           deriving (Eq, Ord, Read, Show)

symbols :: [Char] -> [Regex]
symbols = map Symbol

unions :: [Regex] -> Regex
unions = foldl1 Union

concats :: [Regex] -> Regex
concats = foldl1 Concat

unionChars :: [Char] -> Regex
unionChars = unions . symbols

showRegex :: Regex -> String
showRegex = run where
    run Epsilon = "ε"
    run Endmark = "#"
    run (Symbol ch) = [ch]
    run (Concat r1 r2) = run r1 ++ run r2
    run (Union  r1 r2) = "(" ++ run r1 ++ "|" ++ run r2 ++ ")"
    run (Closure r) = "(" ++ run r ++ ")*"

readRegex :: String -> Regex
readRegex s = case parse (parseRegex M.empty) "" s of
    Left err -> error $ show err
    Right re -> re

parseRegex :: M.Map String Regex -> GenParser Char st Regex
parseRegex symbolTable = regex where
    regex     = try union <|> simpleReg
    union     = simpleReg `chainl1` (char '|' >> return Union)
    simpleReg = try concat <|> basicReg
    concat    = basicReg  `chainl1` (return Concat)
    basicReg  = try star <|> try plus <|> try question <|> elemReg
    star      = elemReg <* char '*' >>= return . Closure
    plus      = elemReg <* char '+' >>= return . \r -> Concat r (Closure r)
    question  = elemReg <* char '?' >>= return . Union Epsilon
    elemReg   = try userDef <|> try symbol <|> try epsilon <|> try any <|> try group <|> set
    userDef   = betweenStrMany "{" "}" anyChar >>= return . (symbolTable !)
    group     = betweenStrOne  "(" ")" regex
    any       = char '.' >> return (unionChars charset)
    epsilon   = char 'ε' >> return Epsilon
    symbol    = character >>= return . Symbol
    character = try meta <|> try escape <|> oneOf (charset \\ (metaChars <> unescapeChars))
    meta      = char '\\' *> oneOf metaChars -- try meta first, or \\* will be parsed into Closure (Symbol '\\')
    escape    = char '\\' *> oneOf escapeChars >>= return . unescape
    set       = try negSet <|> posSet
    posSet    = betweenStrOne "["  "]" setItems >>= return . unionChars
    negSet    = betweenStrOne "[^" "]" setItems >>= return . unionChars . (charset \\)
    setItems  = setItem `chainl1` return L.union
    setItem   = try (return <$> setMeta) <|> try range <|> (return <$> setChar)
    setMeta   = char '\\' *> oneOf ['-', ']']
    setChar   = oneOf (charset \\ ([']'] <> unescapeChars))
    range     = enumFromTo <$> character <* char '-' <*> character

charset :: [Char]
charset = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!#%',/:;<=>{}&_-~\\\"|.*+?()[^]\n\t\v\f "

metaChars :: [Char]
metaChars = "\\|.*+?(){}[]ε"

escapeMap :: M.Map Char Char
escapeMap = M.fromList [('n', '\n'), ('t', '\t'), ('v', '\v'), ('f', '\f')]

escapeChars :: [Char]
escapeChars = M.keys escapeMap

unescapeChars :: [Char]
unescapeChars = M.elems escapeMap

unescape :: Char -> Char
unescape = (escapeMap !)

betweenStrMany :: String -> String -> GenParser Char st a -> GenParser Char st [a]
betweenStrMany open close p = string open >> manyTill p (string close)

betweenStrOne :: String -> String -> GenParser Char st a -> GenParser Char st a
betweenStrOne open close = between (string open) (string close)
