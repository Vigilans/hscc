module Text.Lexer.DFA.Array where

import qualified Text.Lexer.DFA as DFA
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set.Monad as S
import Data.Array ((!))
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Set (elemAt, lookupIndex)
import Text.Lexer.DFA (Condition, Tag)

type State = Int

mapState :: DFA.State -> State
mapState DFA.Empty = 0
mapState s = (elemAt 0 . S.toPreludeSet . DFA.code) s

data ArrayDFA = ArrayDFA {
    initState  :: State,
    encoder    :: A.Array Condition Int,
    transTable :: A.Array (State, Int) State,
    tagsTable  :: A.Array State [Tag]
} deriving (Eq, Read, Show)

encode :: ArrayDFA -> Condition -> Int
encode dfa c = encoder dfa ! c

trans :: ArrayDFA -> State -> Condition -> State
trans dfa s c = transTable dfa ! (s, encode dfa c)

tags :: ArrayDFA -> State -> [Tag]
tags dfa s = tagsTable dfa ! s

accept :: ArrayDFA -> State -> Bool
accept dfa s = tags dfa s /= []

build :: DFA.DFA -> ArrayDFA
build dfa' = let
    dfa@DFA.DFA { DFA.alphabet, DFA.states, DFA.initialState } = DFA.makeIndex dfa' 1 -- index starts from 1
    -- State in ArrayDFA is the index code of the origin state
    initState = mapState initialState
    -- Encoder maps a char input to a reduced condition space (0 represents invalid input)
    encoder = A.listArray (chr 0, chr 127) [ i + 1 |
        c <- [chr 0 .. chr 127],
        let i = fromMaybe (-1) (lookupIndex c . S.toPreludeSet $ alphabet)
        ]
    -- Trans table stores a 2-d table, with row 0 to be empty state & col 0 to be invalid input
    transTable = A.array ((0, 0), (S.size states, S.size alphabet)) [ ((from, c), to) |
        from <- [0 .. S.size states], c <- [0 .. S.size alphabet],
        let to = if from == 0 || c == 0 then 0 -- Empty state or invalid input results in empty state
                 else mapState $ DFA.trans dfa (elemAt (from - 1) $ S.toPreludeSet states) (elemAt (c - 1) $ S.toPreludeSet alphabet)
        ]
    -- Tags table only stores tags of accept states (used for identifying whether a state is accepted)
    tagsTable = A.array (0, S.size states) [ (mapState s, tags) |
        s <- DFA.Empty : S.toAscList states,
        let tags = if DFA.accept dfa s then DFA.tags s else []
        ]
    in ArrayDFA { initState, encoder, transTable, tagsTable }

run :: ArrayDFA -> [Condition] -> ([Tag], ([Condition], [Condition]))
run dfa input = forward (initState dfa) [initState dfa] input where
    forward _ ss [] = backtrack ss
    forward x ss (c:cs) = case trans dfa x c of
        0 -> backtrack ss
        y -> forward y (y:ss) cs
    backtrack [] = ([], ([], input))
    backtrack (s:ss) = case accept dfa s of
        True  -> (tags dfa s, splitAt (length ss) input)
        False -> backtrack ss

showDFA :: ArrayDFA -> String
showDFA ArrayDFA { initState, encoder, transTable, tagsTable } = let
    validChars = [(c, a) | (c, a) <- A.assocs encoder, a /= 0]
    acceptTags = [(s, t) | (s, t) <- A.assocs tagsTable, t /= []]
    maxDigits = length . show . A.rangeSize . A.bounds $ tagsTable
    padding x = replicate (maxDigits - length x) ' ' ++ x
    prepend x acc = concat [padding x, " ", acc]
    (rows, cols) = snd $ A.bounds transTable
    in L.intercalate "\n" [
        "ArrayDFA {",
        "initState  = " ++ show initState,
        "encoder    = " ++ show validChars,
        "tagsTable  = " ++ show acceptTags,
        "transTable = ",
        foldr1 prepend $ " " : map (pure . fst) validChars,
        L.intercalate "\n" [foldr1 prepend $ show r : [if s /= 0 then show s else " " | c <- [1..cols], let s = transTable ! (r, c) ] | r <- [1..rows]],
        "}"]
