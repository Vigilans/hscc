module Text.Lexer.Regex where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Text.ParserCombinators.Parsec hiding (State)

data Regex = Epsilon
           | Symbol Char
           | Union  Regex Regex
           | Concat Regex Regex
           | Closure Regex
           | Endmark
           deriving (Eq, Ord, Read, Show)

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

parseRegex :: M.Map String Regex -> GenParser Char state Regex
parseRegex symbolTable = regex where
    regex       = try union  <|> simpleReg
    union       = simpleReg `chainl1` (Union <$ char '|')
    simpleReg   = try concat <|> basicReg
    concat      = basicReg  `chainl1` (return Concat)

    basicReg    = try star   <|> try plus <|> try question <|> elemReg
    star        = Closure <$> elemReg <* char '*'
    plus        = do
        r <- elemReg
        char '+'
        return $ Concat r (Closure r)
    question    = do
        r <- elemReg
        char '?'
        return $ Union r Epsilon

    elemReg     = try userDef <|> try symbol <|> try epsilon <|> try any <|> try group <|> set
    userDef     = do
        char '{'
        name  <- manyTill anyChar (char '}')
        return $ unpack (M.lookup name symbolTable) where
            unpack (Just regex) = regex
            -- TODO: should raise error here
            -- unpack Nothing = Epsilon
    group       = between (char '(') (char ')') regex
    epsilon     = Epsilon <$  char 'ε'
    any         = do
        char '.'
        return $ foldl1 (\a b -> Union a b) (Symbol <$> alphabet)

    set         = try negSet <|> posSet
    negSet      = do
        string "[^"
        alphas <- setItems
        char ']'
        return $ foldl1 (\a b -> Union a b) (Symbol <$> [ x | x <- alphabet, not $ x `elem` alphas])
    posSet      = do
        char '['
        alphas <- setItems
        char ']'
        return $ foldl1 (\a b -> Union a b) (Symbol <$> alphas)
    setItems    = setItem `chainl1` (return (++))
    setItem     = try range <|> unpack <$> symbol where unpack (Symbol c) = [c]
    range       = do
        l <- symbol
        char '-'
        r <- symbol
        return $ [(unpack l)..(unpack r)] where unpack (Symbol c) = c
    symbol      = try meta <|> try escape <|> Symbol <$> noneOf metaChars
    -- try meta first, otherwise \\* will be parsed into Closure (Symbol '\\')
    meta        = do
        char '\\'
        r <- oneOf metaChars
        return $ Symbol r
    -- note that in '.l' files, '\t' is actually presented as '\\t'
    escape      = do
        char '\\'
        r <- oneOf escapeChars
        return $ Symbol (norm r) where
            norm 'n' = '\n'
            norm 't' = '\t'
            norm 'v' = '\v'
            norm 'f' = '\f'

    metaChars = "\\|.*+?()[^]ε"
    escapeChars = "ntvf"
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!#%',/:;<=>{}&_ ?()[].^*+-~|\n\t\v\f\"\\"
