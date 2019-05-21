module Text.Lexer.DFA.Regex (
    regex2dfa
) where

import Text.Lexer.Regex
import qualified Text.Lexer.DFA as DFA
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Q -- As queue
import Data.Map ((!))
import Data.Sequence (Seq((:<|), (:|>)))
import Control.Monad.State

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
        modify $ \RegexFunction { nullable, firstpos, lastpos, followpos, leafsymb } -> RegexFunction {
            nullable  = M.insert n True nullable,
            firstpos  = M.insert n S.empty firstpos,
            lastpos   = M.insert n S.empty lastpos,
            followpos = followpos,
            leafsymb  = leafsymb
        }
    run n@(Union c1 c2) = do
        run c1
        run c2
        modify $ \RegexFunction { nullable, firstpos, lastpos, followpos, leafsymb } -> RegexFunction {
            nullable  = M.insert n ((nullable ! c1) || (nullable ! c2)) nullable,
            firstpos  = M.insert n ((firstpos ! c1) <> (firstpos ! c2)) firstpos,
            lastpos   = M.insert n ((lastpos  ! c1) <> (lastpos  ! c2)) lastpos,
            followpos = followpos,
            leafsymb  = leafsymb
        }
    run n@(Concat c1 c2) = do
        run c1
        run c2
        modify $ \RegexFunction { nullable, firstpos, lastpos, followpos, leafsymb } -> RegexFunction {
            nullable  = M.insert n ((nullable ! c1) && (nullable ! c2)) nullable,
            firstpos  = M.insert n ((firstpos ! c1) <> (if nullable ! c1 then firstpos ! c2 else S.empty)) firstpos,
            lastpos   = M.insert n ((lastpos  ! c2) <> (if nullable ! c2 then lastpos  ! c1 else S.empty)) lastpos,
            followpos = S.foldr (M.adjust $ S.union (firstpos ! c2)) followpos (lastpos ! c1),
            leafsymb  = leafsymb
        }
    run n@(Closure c) = do
        run c
        modify $ \RegexFunction { nullable, firstpos, lastpos, followpos, leafsymb } -> RegexFunction {
            nullable  = M.insert n True nullable,
            firstpos  = M.insert n (firstpos ! c) firstpos,
            lastpos   = M.insert n (lastpos  ! c) lastpos,
            followpos = S.foldr (M.adjust $ S.union (firstpos ! n)) followpos (lastpos ! n),
            leafsymb  = leafsymb
        }
    run n = do -- Symbol or Endmarker
        poses <- gets leafsymb
        let i = maximum (0 : M.keys poses) + 1
        modify $ \RegexFunction { nullable, firstpos, lastpos, followpos, leafsymb } -> RegexFunction {
            nullable  = M.insert n False nullable,
            firstpos  = M.insert n (S.singleton i) firstpos,
            lastpos   = M.insert n (S.singleton i) lastpos,
            followpos = M.insert i S.empty followpos,
            leafsymb  = M.insert i n leafsymb
        }

data RegexToDFAState = R2DS {
    trans   :: S.Set DFA.Transition,
    states  :: S.Set DFA.State,
    accepts :: S.Set DFA.State
}

regex2dfa :: Regex -> DFA.Tag -> DFA.DFA
regex2dfa reg tag = let
    -- Augment regex with '#'
    aug = Union reg Endmark
    -- Compute regex functions
    RegexFunction { firstpos, followpos, leafsymb } = regexFunction aug
    -- Prepare DFA invariants and utility functions
    inputSymbs = [s | Symbol s <- M.elems leafsymb]
    endmarkPos = head [i | (i, Endmark) <- M.assocs leafsymb]
    isAccept   = S.member endmarkPos
    newState c = DFA.State c [tag | isAccept c]
    initState  = newState (firstpos ! aug)
    -- Run a queue to generate DFA
    run :: Q.Seq DFA.State -> State RegexToDFAState DFA.DFA
    run Q.Empty = gets $ \(R2DS ts ss as) -> DFA.build (ts, initState, as)
    run (s :<| rest) = do
        queue <- foldM (\queue a -> do
            R2DS { trans, states, accepts } <- get
            let sCode = S.toList (DFA.code s)
                uCode = S.unions [followpos ! p | p <- sCode, leafsymb ! p == Symbol a]
                u = newState uCode
            -- Add new transition
            modify $ \r2ds -> r2ds { trans = S.insert (s, a, u) trans }
            -- Check whether it is a new state
            if S.notMember u states then do
                -- Add to total states
                modify $ \r2ds -> r2ds { states = S.insert u states }
                -- Check whether it is an accept state
                when (isAccept uCode) $
                    modify $ \r2ds -> r2ds { accepts = S.insert u accepts }
                -- Add new state to queue
                return (queue :|> u)
            else return queue) rest inputSymbs
        run queue
    in evalState (run $ Q.singleton initState) (R2DS S.empty S.empty S.empty)
