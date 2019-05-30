module Text.Lexer.DFA where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set.Monad as S
import Data.Set (findIndex)
import Data.Map ((!), (!?))
import Data.Set.Monad ((\\))
import Data.Maybe
import Control.Monad
import Control.Monad.State hiding (State)

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
    -- a <> b = {-eliminateDeads. minimize .-} reduction $ crossUnion a b
    a <> b = dfsUnions [a, b]

instance Monoid DFA where
    mempty = empty
    mconcat = dfsUnions

build :: (S.Set Transition, State, S.Set State) -> DFA
build (transitions, initialState, acceptStates) = let
    (alphabet, states, transTable) = S.foldl (\(conds, states, table) (from, cond, to) -> (
            conds  <> pure cond,
            states <> pure from <> pure to,
            M.insert (from, cond) to table
        )) (S.empty, acceptStates <> pure initialState, M.empty) transitions
    in DFA alphabet (states \\ pure Empty) initialState acceptStates transTable

run :: DFA -> [Condition] -> ([Tag], ([Condition], [Condition]))
run dfa input = forward (initialState dfa) [initialState dfa] input where
    forward _ ss [] = backtrack ss
    forward x ss (c:cs) = case trans dfa x c of
        Empty -> backtrack ss
        y -> forward y (y:ss) cs
    backtrack [] = ([], ([], input))
    backtrack (s:ss) = case accept dfa s of
        True  -> (tags s, splitAt (length ss) input)
        False -> backtrack ss

test :: DFA -> [Condition] -> Bool
test dfa = accept dfa . foldl (trans dfa) (initialState dfa)

type Group = S.Set State
type Partition = [Group]

mapStates :: (State -> State) -> DFA -> DFA
mapStates f dfa@DFA { alphabet, states, initialState, acceptStates, transTable } = let
    f' s = if s /= Empty then f s else Empty
    initialState' = f' initialState
    acceptStates' = fmap f' acceptStates \\ pure Empty
    mapAlphabet s = [(f' s, c, f' s') | c <- alphabet, let s' = trans dfa s c, f' s /= Empty && f' s' /= Empty]
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

eliminateDeads :: DFA -> DFA
eliminateDeads dfa@ DFA { alphabet, states } = let
    -- Dead state is one that not accepting and transfers to itself on each input symbol
    isDead s = not (accept dfa s) && (trans dfa s <$> alphabet) `S.isSubsetOf` S.fromList [s, Empty]
    deadStates = S.filter isDead states
    eliminated = mapStates (\s -> if s `S.member` deadStates then Empty else s) dfa
    -- Recursive apply until no more dead state. This could also eliminate those states that cannot lead to accept state
    in if null deadStates then dfa else eliminateDeads eliminated

partition :: DFA -> Partition -> Partition
partition dfa groups = run [] groups where
    findGroup s = L.findIndex (s `elem`) groups -- State mapped to different [Group] is distinguishable
    appendTag s = (, if s /= Empty then tags s else []) -- State of different tags is distinguishable
    run front [] = front
    run front (group:back) = run (front ++ subgroups) back where
        mapper s = appendTag s . findGroup . trans dfa s <$> alphabet dfa
        folder s = M.insertWith (<>) (mapper s) (pure s)
        subgroups = M.elems $ S.foldr folder M.empty group

minimize :: DFA -> DFA
minimize dfa@DFA { states, acceptStates } = mapStates repState dfa where
    -- Split until partition can be no more fine
    atomicSplit :: Partition -> Partition
    atomicSplit cur = if cur == new then cur else atomicSplit new where new = partition dfa cur
    {- Get final partition. There are two initial partition chocies:
       - F, S - F, and {Empty}: This will not eliminate the dead state.
       - F, {Empty} ∪ (S - F): This will eliminate the dead state, as Empty is the smallest state,
         and will be chosen as reprentative state in the dead state group. -}
    finalPartition = atomicSplit [acceptStates, pure Empty <> states \\ acceptStates]
    -- Get representative state (it must exists)
    repState :: State -> State
    repState s = fromJust $ S.findMin <$> L.find (s `S.member`) finalPartition

makeIndex :: DFA -> Int -> DFA
makeIndex dfa offset = mapStates (\s -> s { code = S.singleton (getIndex s + offset) }) dfa
    where getIndex s = findIndex s (S.toPreludeSet $ states dfa)

crossUnion :: DFA -> DFA -> DFA
crossUnion a' b' = let
    a = makeIndex a' 0
    b = makeIndex b' (S.size $ states a)
    transitions = [ (from, c, to) | -- Union using Cartesian Product
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

dfsUnions :: [DFA] -> DFA
dfsUnions dfas = let
    -- Prepare invariants before union
    inds  = scanl (\i dfa -> i + S.size (states dfa)) 0 dfas
    dfas' = map (uncurry makeIndex) (zip dfas inds)
    alphabet' = mconcat $ alphabet <$> dfas'
    initState' = initialState <$> dfas'
    -- Concurrently trans state tuple to a new state tuple
    trans' :: [State] -> Condition -> [State]
    trans' ss c = uncurry trans <$> zip dfas' ss <*> [c]
    -- If any state in the state tuple is accepted, then accept the union state
    accept' :: [State] -> Bool
    accept' ss = or (uncurry accept <$> zip dfas' ss)
    -- Concurrently DFS traversal to build union DFA
    dfs [] = gets $ \(ts, as, _) -> build (ts, mconcat initState', as)
    dfs (s:ss) = do
        let u = mconcat s -- u is the unioned state of state tuple s
        -- Get new transitions and children to traverse
        (transis, children) <- gets $ \(_, _, visited) -> S.foldl (\(ts, cs) c ->
            let s' = trans' s c; u' = mconcat s' in (
                ts <> [(u, c, u') | u /= Empty && u' /= Empty],
                cs <> [s' | u' `S.notMember` visited, u' /= Empty]
            )) (S.empty, []) alphabet'
        -- Modify the union DFA building state
        modify $ \(ts, as, vs) -> (ts <> transis, as <> [u | accept' s], vs <> pure u)
        -- Recursively traverse non-visited states
        dfs (children ++ ss)
    in evalState (dfs [initState']) (S.empty, S.empty, S.empty)
