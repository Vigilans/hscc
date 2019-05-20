module Text.Lexer.DFA where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Map ((!), (!?))
import Data.Set ((\\))

data State = Empty | State {
    code :: S.Set Int,
    tags :: [Tag]
} deriving (Eq, Ord, Read, Show)

instance Semigroup State where
    Empty <> b = b
    a <> Empty = a
    a <> b = State (code a <> code b) (tags a <> tags b)

type Tag = String
type Condition  = Char
type Transition = (State, Condition, State)

data DFA = DFA {
    alphabet     :: S.Set Condition,
    states       :: S.Set State,
    initialState :: State,
    acceptStates :: S.Set State,
    transTable   :: M.Map (State, Condition) State
} deriving (Eq, Read, Show)

trans :: DFA -> State -> Condition -> State
trans dfa s c = fromMaybe Empty $ transTable dfa !? (s, c)

accept :: DFA -> State -> Bool
accept dfa s = S.member s $ acceptStates dfa

empty :: DFA
empty = DFA S.empty S.empty Empty S.empty M.empty

build :: ([Transition], State, S.Set State) -> DFA
build (transitions, initialState, acceptStates) = let
    (alphabet, states, transTable) = foldl (\(conds, states, table) (from, cond, to) -> (
            S.insert cond conds,
            foldr S.insert states [from, to],
            M.insert (from, cond) to table
        )) (S.empty, S.empty, M.empty) transitions
    in DFA { alphabet, states, initialState, acceptStates, transTable }

run :: DFA -> [Condition] -> [Tag]
run dfa input = go (initialState dfa) input [] where
    go _ [] ts = ts
    go x (c:cs) ts = case trans dfa x c of
        Empty -> ts
        y -> go y cs (if accept dfa y then tags y else ts)

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
    f' s = if s /= Empty then f s else Empty
    mapAlphabet :: State -> M.Map (State, Condition) State
    mapAlphabet s = M.fromList [((s, c), f' $ trans dfa s c) | c <- S.toList alphabet]
    initialState' = f' initialState
    acceptStates' = S.map f' acceptStates \\ S.singleton Empty
    states'       = S.map f' states \\ S.singleton Empty
    transTable'   = S.foldr (M.union . mapAlphabet) M.empty states'
    alphabet'     = S.map snd $ M.keysSet transTable'
    in DFA alphabet' states' initialState' acceptStates' transTable'

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

makeIndex :: DFA -> Int -> DFA
makeIndex dfa offset = mapStates (\s -> s { code = S.singleton (S.findIndex s (states dfa) + offset) }) dfa

union :: DFA -> DFA -> DFA
union a b = let
    a = makeIndex a 0
    b = makeIndex b (S.size $ states a)
    transitions = [ (from, c, to) |
        c <- S.toList (alphabet a <> alphabet b),
        x <- Empty : S.toList (states a),
        y <- Empty : S.toList (states b),
        let from = x <> y; to = trans a x c <> trans b y c,
        to /= Empty
        ]
    initial = initialState a <> initialState b
    accepts = S.fromList [ x <> y |
        x <- Empty : S.toList (states a),
        y <- Empty : S.toList (states b),
        S.member x (acceptStates a) || S.member y (acceptStates b)
        ]
    in build (transitions, initial, accepts)

unions :: [DFA] -> DFA
unions = foldl union empty
