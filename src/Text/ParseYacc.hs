module Text.ParseYacc where
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Control.Monad

data Terminal = Terminal String | Epsilon | Eof deriving (Eq, Ord, Show)
data NonTerminal = StartRule | NonTerminal String deriving (Eq, Ord, Show)
data Symbol = STerminal Terminal | SNonTerminal NonTerminal deriving (Eq, Ord, Show)

data RawParser = RawParser {
    terminals :: [Terminal],
    nonTerminals :: [NonTerminal],
    rules :: M.Map NonTerminal [[Symbol]],
    start :: NonTerminal
    userCode :: String
} deriving (Show)

-- hmm... just a demo...
parseYacc :: GenParser Char RawParser RawParser
parseYacc = do
    skipMany trim
    -- Parse Tokens
    manyTill (try trim <|> try parseUserDefToken <|> parseStart) (try $ string "%%")
    -- Parse Rules
    manyTill (try trim <|> parseRule) (try $ string "%%")
    -- Parse User Code
    userCode <- manyTill anyChar eof
    updateState $ \parser -> parser { userCode }
    getState

parseUserDefToken :: GenParser Char RawParser ()
parseUserDefToken = do
    string "%token"
    manyTill (try trim <|> parseToken) (try $ string "\n")    

parseToken :: GenParser Char RawParser ()
parseToken = do
    RawParser { terminals } <- getState
    token  <- many (noneOf [' ', '\n', '\t'])
    updateState $ \parser -> parser {
        terminals = (Terminal token):terminals
    }

parseUserDefToken :: GenParser Char RawParser ()
parseUserDefToken = do
    RawParser { start } <- getState
    string "%start"
    startName  <- manyTill anyChar (try $ string "\n")    
    updateState $ \parser -> parser {
        start = NonTerminal startName
    }

parseRule :: GenParser Char RawParser ()
parseRule = undefined

betweenStrMany :: String -> String -> GenParser Char st a -> GenParser Char st [a]
betweenStrMany open close p = string open >> manyTill p (string close)

betweenStrOne :: String -> String -> GenParser Char st a -> GenParser Char st a
betweenStrOne open close = between (string open) (string close)
trim :: GenParser Char st ()
trim = try (void space) <|> void comment -- Trim spaces and comments

comment :: GenParser Char st String
comment = betweenStrMany "/*" "*/" anyChar