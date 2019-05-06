module Text.Lexer.DFA where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set.Monad as S
import qualified Data.Array
import Data.Maybe
import Data.Map ((!))
import Data.Set.Monad ((\\))

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

trans :: DFA -> State -> Condition -> State
trans dfa s c = transTable dfa ! (s, c)

build :: ([Transition], State, S.Set State) -> DFA
build (transitions, initialState, acceptStates) = let
    (alphabet, states, transTable) = foldl (\(conds, states, table) (from, cond, to) -> (
            S.insert cond conds,
            foldr S.insert states [from, to],
            M.insert (from, cond) to table
        )) (S.empty, S.empty, M.empty) transitions
    in DFA { alphabet, states, initialState, acceptStates, transTable }

type Group = S.Set State
type Partition = [Group]

partition :: DFA -> Partition -> Partition
partition dfa = run [] where
    run front [] = front
    run front (group:back) = run (front ++ subgroups) back where
        mapper s = S.map (trans dfa s) (alphabet dfa)
        folder s = M.insertWith S.union (mapper s) (S.singleton s)
        subgroups = M.elems $ S.foldr folder M.empty group

mapStates :: (State -> State) -> DFA -> DFA
mapStates f DFA { alphabet, states, initialState, acceptStates, transTable } = let
    mapAlphabet :: State -> M.Map (State, Condition) State
    mapAlphabet s = M.fromList [((s, c), transTable ! (s, c)) | c <- S.toList alphabet]
    initialState = f initialState
    acceptStates = S.map f acceptStates
    states       = S.map f states
    transTable   = S.foldr (M.union . mapAlphabet) M.empty states
    in DFA { alphabet, states, initialState, acceptStates, transTable }

minimize :: DFA -> DFA
minimize dfa@DFA { states, acceptStates } = mapStates repState dfa where
    -- Split until partition can be no more fine
    atomicSplit :: Partition -> Partition
    atomicSplit cur = if cur == new then cur else atomicSplit new where new = partition dfa cur
    -- Get final partition
    finalPartition = atomicSplit [acceptStates, states \\ acceptStates]
    -- Get representative state (it must exists)
    repState :: State -> State
    repState s = fromJust $ S.findMin <$> L.find (S.member s) finalPartition
