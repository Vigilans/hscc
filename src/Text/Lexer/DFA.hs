module Text.Lexer.DFA where

import qualified Data.Map as M
import qualified Data.Set.Monad as S
import qualified Data.Array

type State      = S.Set Int
type Condition  = Char
type Transition = (State, Condition, State)

data DFA = DFA {
    alphabet     :: S.Set Condition,
    states       :: S.Set State,
    initialState :: State,
    acceptStates :: S.Set State,
    transTable   :: M.Map (State, Condition) State
}

build :: ([Transition], State, S.Set State) -> DFA
build (transitions, initialState, acceptStates) = let
    (alphabet, states, transTable) = foldl (\(conds, states, table) (from, cond, to) -> (
            S.insert cond conds,
            foldr S.insert states [from, to],
            M.insert (from, cond) to table
        )) (S.empty, S.empty, M.empty) transitions
    in DFA { alphabet, states, initialState, acceptStates, transTable }
