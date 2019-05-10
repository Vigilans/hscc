module Text.Lexer.Regex (
    Regex (..),
    regex2dfa,
    showRegex,
    readRegex,
    parseRegex
) where


import qualified Text.Lexer.DFA as DFA
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set.Monad as S
import qualified Data.Sequence as Q -- As queue
import Data.Map ((!))
import Data.Sequence (Seq((:<|), (:|>)))
import Control.Monad.State
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
readRegex s = case parse parseRegex "" s of
    Left err -> error $ show err
    Right re -> re

parseRegex :: Parser Regex
parseRegex = regex where
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
    elemReg     = try symbol <|> try epsilon <|> try any <|> group
    group       = between (char '(') (char ')') regex
    epsilon     = Epsilon <$  char 'ε'
    any         = do
        char '.'
        return $ foldl (\a b -> Concat a b) (Concat (Symbol 'A') (Symbol 'B')) (Symbol <$> (tail $ tail alphabet))
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
    metaChars = "\\|.*+?()ε"
    escapeChars = "ntvf"    
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!\"#%'()*+,-./:;<=>?[\\]^{|}_ \n\t\v\f~&"

type Position = Int
type Positions = S.Set Int

data RegexFunction = RegexFunction {
    nullable  :: M.Map Regex Bool,
    firstpos  :: M.Map Regex Positions,
    lastpos   :: M.Map Regex Positions,
    followpos :: M.Map Position Positions,
    leafsymb  :: M.Map Position Regex
}

-- Post-order traversal
regexFunction :: Regex -> RegexFunction
regexFunction re = execState (run re) (RegexFunction M.empty M.empty M.empty M.empty M.empty) where
    run n@Epsilon =
        modify $ \s -> RegexFunction {
            nullable  = M.insert n True $ nullable s,
            firstpos  = M.insert n S.empty $ firstpos s,
            lastpos   = M.insert n S.empty $ lastpos s,
            followpos = followpos s,
            leafsymb  = leafsymb s
        }
    run n@(Union c1 c2) = do
        run c1
        run c2
        modify $ \s -> RegexFunction {
            nullable  = M.insert n ((nullable s ! c1) || (nullable s ! c2)) $ nullable s,
            firstpos  = M.insert n ((firstpos s ! c1) `S.union` (firstpos s ! c2)) $ firstpos s,
            lastpos   = M.insert n ((lastpos  s ! c1) `S.union` (lastpos  s ! c2)) $ lastpos s,
            followpos = followpos s,
            leafsymb  = leafsymb s
        }
    run n@(Concat c1 c2) = do
        run c1
        run c2
        modify $ \s -> RegexFunction {
            nullable  = M.insert n ((nullable s ! c1) && (nullable s ! c2)) $ nullable s,
            firstpos  = M.insert n ((firstpos s ! c1) `S.union` (if nullable s ! c1 then firstpos s ! c2 else S.empty)) $ firstpos s,
            lastpos   = M.insert n ((lastpos  s ! c2) `S.union` (if nullable s ! c2 then lastpos  s ! c1 else S.empty)) $ lastpos s,
            followpos = S.foldr (M.adjust $ S.union (firstpos s ! c2)) (followpos s) (lastpos s ! c1),
            leafsymb  = leafsymb s
        }
    run n@(Closure c) = do
        run c
        modify $ \s -> RegexFunction {
            nullable  = M.insert n True $ nullable s,
            firstpos  = M.insert n (firstpos s ! c) $ firstpos s,
            lastpos   = M.insert n (lastpos  s ! c) $ lastpos s,
            followpos = S.foldr (M.adjust $ S.union (firstpos s ! n)) (followpos s) (lastpos s ! n),
            leafsymb  = leafsymb s
        }
    run n = do -- Symbol or Endmarker
        poses <- gets leafsymb
        let i = maximum (0 : M.keys poses) + 1
        modify $ \s -> RegexFunction {
            nullable  = M.insert n False $ nullable s,
            firstpos  = M.insert n (S.singleton i) $ firstpos s,
            lastpos   = M.insert n (S.singleton i) $ lastpos s,
            followpos = M.insert i S.empty $ followpos s,
            leafsymb  = M.insert i n $ leafsymb s
        }

data Regex2DFAState = R2DS {
    trans   :: [DFA.Transition],
    states  :: S.Set DFA.State,
    accepts :: S.Set DFA.State
}

regex2dfa :: Regex -> DFA.DFA
regex2dfa reg = let
    -- Augment regex with '#'
    aug = Union reg Endmark
    -- Compute regex functions
    RegexFunction { firstpos, followpos, leafsymb } = regexFunction aug
    -- Prepare DFA invariants
    initState  = firstpos ! aug
    inputSymbs = [s | Symbol s <- M.elems leafsymb]
    endmarkPos = head [i | (i, Endmark) <- M.assocs leafsymb]
    -- Run a queue to generate DFA
    run :: Q.Seq DFA.State -> State Regex2DFAState DFA.DFA
    run Q.Empty = gets $ \(R2DS ts ss as) -> DFA.build (ts, initState, as)
    run (s :<| rest) = do
        queue <- foldM (\queue a -> do
            R2DS { trans, states, accepts } <- get
            let u = S.unions [followpos ! p | p <- S.toList s, leafsymb ! p == Symbol a]
            -- Add new transition
            modify $ \r2ds -> r2ds { trans = (s, a, u) : trans }
            -- Check whether it is a new state
            if S.notMember u states then do
                -- Add to total states
                modify $ \r2ds -> r2ds { states = S.insert u states }
                -- Check whether it is an accept state
                when (S.member endmarkPos u) $
                    modify $ \r2ds -> r2ds { accepts = S.insert u accepts }
                -- Add new state to queue
                return (queue :|> u)
            else return queue) rest inputSymbs
        run queue
    in evalState (run $ Q.singleton initState) (R2DS [] S.empty S.empty)
