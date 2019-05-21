module Text.Lexer.DFA.Regex where

import Text.Lexer.Regex
import qualified Text.Lexer.DFA as DFA
import qualified Data.Map as M
import qualified Data.Set.Monad as S
import qualified Data.Sequence as Q -- As queue
import Data.Map ((!))
import Data.Sequence (Seq((:<|), (:|>)))
import Control.Monad.State

type Position = Int
type Positions = S.Set Position

data RegexAttr = RegexAttr {
    nullable :: Bool,
    firstpos :: Positions,
    lastpos  :: Positions
}

data RegexPose = RegexPose {
    followpos :: M.Map Position Positions,
    leafsymb  :: M.Map Position Regex
}

-- Post-order traversal
regexFunction :: Regex -> (RegexAttr, RegexPose)
regexFunction re = runState (run re) (RegexPose M.empty M.empty) where
    run n@Epsilon =
        return RegexAttr {
            nullable = True,
            firstpos = S.empty,
            lastpos  = S.empty
        }
    run n@(Union c1' c2') = do
        c1 <- run c1'
        c2 <- run c2'
        return RegexAttr {
            nullable = nullable c1 || nullable c2,
            firstpos = firstpos c1 <> firstpos c2,
            lastpos  = lastpos  c1 <> lastpos  c2
        }
    run n@(Concat c1' c2') = do
        c1 <- run c1'
        c2 <- run c2'
        modify $ \rf@RegexPose { followpos } -> rf {
            followpos = S.foldr (M.adjust (firstpos c2 <>)) followpos (lastpos c1)
        }
        return RegexAttr {
            nullable = nullable c1 && nullable c2,
            firstpos = firstpos c1 <> join [firstpos c2 | nullable c1],
            lastpos  = lastpos  c2 <> join [lastpos  c1 | nullable c2]
        }
    run n@(Closure c') = do
        c <- run c'
        modify $ \rf@RegexPose { followpos } -> rf {
            followpos = S.foldr (M.adjust (firstpos c <>)) followpos (lastpos c)
        }
        return RegexAttr {
            nullable = True,
            firstpos = firstpos c,
            lastpos  = lastpos  c
        }
    run n = do -- Symbol or Endmarker
        poses <- gets leafsymb
        let i = maximum (0 : M.keys poses) + 1
        modify $ \rf@RegexPose { followpos, leafsymb } -> rf {
            followpos = M.insert i S.empty followpos,
            leafsymb  = M.insert i n leafsymb
        }
        return RegexAttr {
            nullable  = False,
            firstpos  = pure i,
            lastpos   = pure i
        }

data RegexToDFAState = R2DS {
    trans   :: S.Set DFA.Transition,
    states  :: S.Set DFA.State,
    accepts :: S.Set DFA.State
}

augment :: Regex -> Regex
augment r = Concat r Endmark

regex2dfa :: Regex -> DFA.Tag -> DFA.DFA
regex2dfa reg tag = let
    -- Augment regex with '#'
    aug = augment reg
    -- Compute regex functions
    (RegexAttr{ firstpos }, RegexPose{ followpos, leafsymb }) = regexFunction aug
    -- Prepare DFA invariants and utility functions
    inputSymbs = [s | Symbol s <- M.elems leafsymb]
    endmarkPos = head [i | (i, Endmark) <- M.assocs leafsymb]
    isAccept   = S.member endmarkPos
    newState c = DFA.State c [tag | isAccept c]
    initState  = newState firstpos
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
