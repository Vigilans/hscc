module Text.Lexer where

import qualified Data.Map as M
import qualified Control.Monad.State as MS
import Text.Lexer.Regex
import Text.Lexer.DFA (DFA, Tag)
import Text.Lexer.DFA.Regex
import Text.Lexer.DFA.Array as DFA hiding (State)
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Control.Monad.State hiding (State)
import Data.Map ((!))

type Token = String

data State u = State {
    yystate :: u,
    yyinput :: String,
    yytoken :: Token
}

fromInputState :: (String, u) -> State u
fromInputState (i, s) = State { yyinput = i, yystate = s, yytoken = "" }

toInputState :: State u -> (String, u)
toInputState State{..} = (yyinput, yystate)

type Action s a = MS.State (State s) (Maybe a) -- An action may returns nothing

data Lexer s a = Lexer {
    dfa :: ArrayDFA,
    actions :: M.Map Tag (Action s a)
}

runLexer :: Lexer s a -> String -> s -> (a, (String ,s))
runLexer Lexer{..} input state = let
    run = do
        input <- gets yyinput
        let (match : _, (token, rest)) = DFA.run dfa input
        modify $ \s -> s { yytoken = token, yyinput = rest }
        actions ! match >>= \case
            Nothing -> run
            Just a  -> return a
    in toInputState <$> runState run (fromInputState (input, state))


buildLexer :: (String, String) -> Lexer s a
buildLexer (filename, text) = let
    rawLexer = case runParser parseLexer (RawLexer "" mempty mempty mempty "") filename text of
        Left err -> error $ show err
        Right lexer -> lexer
    in Lexer {  }

{- -------- Lexer metadata by parsing raw text -------- -}

data RawLexer = RawLexer {
    userDefs :: String,
    regexDef :: M.Map String Regex,  -- RE name to Regex
    regexAct :: M.Map String String, -- RE literal to Action
    regexDFA :: DFA,
    userCode :: String
} deriving (Show)

parseLexer :: GenParser Char RawLexer RawLexer
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

parseRegexDef :: GenParser Char RawLexer ()
parseRegexDef = do
    RawLexer { regexDef } <- getState
    name  <- manyTill anyChar space
    _     <- many trim
    regex <- parseRegex regexDef
    updateState $ \lexer -> lexer {
        regexDef = M.insert name regex regexDef
    }

parseRegexAct :: GenParser Char RawLexer ()
parseRegexAct = do
    RawLexer { regexDef } <- getState
    literal <- lookAhead $ manyTill anyChar space
    regex   <- parseRegex regexDef
    _       <- many trim
    action  <- manyTill anyChar endOfLine
    updateState $ \lexer@RawLexer { regexAct, regexDFA } -> lexer {
        regexAct = M.insert literal action regexAct,
        regexDFA = regex2dfa literal regex <> regexDFA -- Use literal as tag
    }

trim :: GenParser Char st ()
trim = try (void space) <|> void comment -- Trim spaces and comments

comment :: GenParser Char st String
comment = betweenStrMany "/*" "*/" anyChar
