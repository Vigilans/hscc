module Text.Parser (
    parser
  , Terminal(..)
  , NonTerminal(..)
  , Symbol(..)
  , Rule
  , Rules
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec as P
import Control.Monad
-- import Text.Regex.Posix

data Terminal = Terminal String | Epsilon | Eof deriving (Eq, Ord, Show)
data NonTerminal = StartRule | NonTerminal String deriving (Eq, Ord, Show)
data Symbol = STerminal Terminal | SNonTerminal NonTerminal deriving (Eq, Ord, Show)

type Rule = (NonTerminal, [[Symbol]])
type Rules = M.Map NonTerminal [[Symbol]]


parser :: String -> Either ParseError Rules
parser = parse ((rules <* spaces) <* eof) "input"

rules :: Parser Rules
rules = M.fromListWith (++) . addStart <$> manyTill (rule <* optional newline) (void newline <|> eof)

rule :: Parser Rule
rule = (,) <$> (nonTerminal <* (string "->") <* spaces') <*> (many symbol `sepBy` (char '|' <* spaces'))

nonTerminal :: Parser NonTerminal
nonTerminal = (NonTerminal <$> ((:) <$> upper <*> many someChar)) <* spaces'

terminal :: Parser Terminal
terminal = (
      Terminal <$> ((:) <$> lower <*> many someChar)
  <|> Terminal <$> (char '"' *> many (noneOf "\"") <* char '"')
  ) <* spaces'

symbol :: Parser Symbol
symbol = (SNonTerminal <$> nonTerminal) <|> (STerminal <$> terminal)

spaces' :: Parser ()
spaces' = skipMany $ char ' '

someChar :: Parser Char
someChar = noneOf [' ', '\n']

-- assume the first rule to be the start rule
addStart :: [Rule] -> [Rule]
addStart xs@((h, _):_) = (StartRule, [[SNonTerminal h, STerminal Eof]]):xs
addStart xs = xs
