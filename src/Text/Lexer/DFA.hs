module Text.Lexer.DFA where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import Data.Maybe
import Data.Map ((!), (!?))
import Data.Set ((\\))

data State = Empty | State {
    code :: S.Set Int,
    tags :: [String]
} deriving (Eq, Ord, Read, Show)

type Condition  = Char
type Transition = (State, Condition, State)

data DFA = DFA {
    alphabet     :: S.Set Condition,
    states       :: S.Set State,
    initialState :: State,
    acceptStates :: S.Set State,
    transTable   :: M.Map (State, Condition) State
} deriving (Read, Show)

trans :: DFA -> State -> Condition -> State
trans dfa s c = fromMaybe Empty $ transTable dfa !? (s, c)

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
mapStates f dfa@DFA { alphabet, states, initialState, acceptStates, transTable } = let
    mapAlphabet :: State -> M.Map (State, Condition) State
    mapAlphabet s = M.fromList [((s, c), f $ trans dfa s c) | c <- S.toList alphabet]
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

makeIndex :: DFA -> DFA -- Starts from 1
makeIndex dfa@DFA { states } = mapStates (\s -> s { code = S.singleton (S.findIndex s states + 1) }) dfa

unionState :: State -> State -> State
unionState = run where
    run Empty Empty = Empty
    run a     Empty = run a (State (S.singleton 0) [])
    run Empty b     = run (State (S.singleton 0) []) b
    run a     b     = State (code a `S.union` code b) (tags a ++ tags b)

union :: DFA -> DFA -> DFA
union a b = let
    a = makeIndex a
    b = makeIndex b
    transitions = [ (from, c, to) |
        c <- S.toList (alphabet a `S.union` alphabet b),
        x <- Empty : S.toList (states a),
        y <- Empty : S.toList (states b),
        let from = unionState x y; to = unionState (trans a x c) (trans b y c),
        to /= Empty
        ]
    initial = unionState (initialState a) (initialState b)
    accepts = S.fromList [ unionState x y |
        x <- Empty : S.toList (states a),
        y <- Empty : S.toList (states b),
        S.member x (acceptStates a) || S.member y (acceptStates b)
        ]
    in makeIndex $ build (transitions, initial, accepts)
