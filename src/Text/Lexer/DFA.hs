module Text.Lexer.DFA where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set.Monad as S
import Data.Set (findIndex)
import Data.Map ((!), (!?))
import Data.Set.Monad ((\\))
import Data.Maybe
import Control.Monad

data State = Empty | State {
    code :: S.Set Int,
    tags :: [Tag]
} deriving (Eq, Ord)

instance Semigroup State where
    Empty <> b = b
    a <> Empty = a
    a <> b = State (code a <> code b) (tags a <> tags b)

instance Monoid State where
    mempty = Empty

instance Show State where
    show Empty = "<.>"
    show (State c ts) = let
        code' = L.intercalate "," (show <$> S.toList c)
        tags' = ";<" ++ L.intercalate "," ts ++ ">"
        in "<" ++ code' ++ (if null ts then "" else tags') ++ ">"

type Tag = String
type Condition  = Char
type Transition = (State, Condition, State)

data DFA = DFA {
    alphabet     :: S.Set Condition,
    states       :: S.Set State,
    initialState :: State,
    acceptStates :: S.Set State,
    transTable   :: M.Map (State, Condition) State
} deriving (Eq, Show)

trans :: DFA -> State -> Condition -> State
trans dfa s c = fromMaybe Empty $ transTable dfa !? (s, c)

accept :: DFA -> State -> Bool
accept dfa s = S.member s $ acceptStates dfa

empty :: DFA
empty = DFA S.empty S.empty Empty S.empty M.empty

instance Semigroup DFA where
    (<>) = union

instance Monoid DFA where
    mempty = empty

build :: (S.Set Transition, State, S.Set State) -> DFA
build (transitions, initialState, acceptStates) = let
    (alphabet, states, transTable) = S.foldl (\(conds, states, table) (from, cond, to) -> (
            S.insert cond conds,
            foldr S.insert states [from, to],
            M.insert (from, cond) to table
        )) (S.empty, S.empty, M.empty) transitions
    in DFA { alphabet, states, initialState, acceptStates, transTable }

run :: DFA -> [Condition] -> [Tag]
run dfa@DFA { initialState } input = go initialState input initTags where
    initTags = if accept dfa initialState then tags initialState else []
    go _ [] ts = ts
    go x (c:cs) ts = case trans dfa x c of
        Empty -> ts
        y -> go y cs (if accept dfa y then tags y else ts)

test :: DFA -> [Condition] -> Bool
test dfa = accept dfa . foldl (trans dfa) (initialState dfa)

type Group = S.Set State
type Partition = [Group]

mapStates :: (State -> State) -> DFA -> DFA
mapStates f dfa@DFA { alphabet, states, initialState, acceptStates, transTable } = let
    f' s = if s /= Empty then f s else Empty
    initialState' = f' initialState
    acceptStates' = fmap f' acceptStates \\ pure Empty
    mapAlphabet s = [(f' s, c, f' s') | c <- alphabet, let s' = trans dfa s c, f' s' /= Empty]
    transitions   = join $ S.map mapAlphabet states
    in build (transitions, initialState', acceptStates')

reduction :: DFA -> DFA
reduction dfa@DFA { alphabet, initialState } = let
    -- Map state to its neighbor to-states
    mapTrans :: State -> S.Set State
    mapTrans s = fmap (trans dfa s) alphabet \\ pure Empty
    -- DFS graph traversal to get visited states
    dfs :: [State] -> S.Set State -> S.Set State
    dfs [] visited = visited
    dfs (s:ss) visited = let
        children = S.toList $ mapTrans s \\ visited
        in dfs (children ++ ss) (S.insert s visited)
    -- Get reachable states from initial state
    reachables = dfs [initialState] S.empty
    -- Reduce all the states that's not reachable from initial state
    in mapStates (\s -> if s `S.member` reachables then s else Empty) dfa

partition :: DFA -> Partition -> Partition
partition dfa = run [] where
    run front [] = front
    run front (group:back) = run (front ++ subgroups) back where
        mapper s = trans dfa s <$> alphabet dfa
        folder s = M.insertWith (<>) (mapper s) (pure s)
        subgroups = M.elems $ S.foldr folder M.empty group

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
makeIndex dfa offset = mapStates (\s -> s { code = S.singleton (getIndex s + offset) }) dfa
    where getIndex s = findIndex s (S.toPreludeSet $ states dfa)

union :: DFA -> DFA -> DFA
union a' b' = let
    a = makeIndex a' 0
    b = makeIndex b' (S.size $ states a)
    transitions = [ (from, c, to) |
        c <- alphabet a <> alphabet b,
        x <- states a <> pure Empty,
        y <- states b <> pure Empty,
        let from = x <> y; to = trans a x c <> trans b y c, to /= Empty
        ]
    initial = initialState a <> initialState b
    accepts = [ x <> y |
        x <- states a <> pure Empty,
        y <- states b <> pure Empty,
        S.member x (acceptStates a) || S.member y (acceptStates b)
        ]
    in build (transitions, initial, accepts)

unions :: [DFA] -> DFA
unions = mconcat
