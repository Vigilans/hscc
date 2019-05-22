module Text.Lexer.DFASpec where

import Test.Hspec
import Test.QuickCheck
import Text.Lexer.DFA
import qualified Data.Set.Monad as S
import qualified Data.Map as M

-- Helper state function
state :: [Int] -> [String] -> State
state code tags = State { code = S.fromList code, tags }

stateTest :: [Int] -> State
stateTest code = state code ["test"]

makeTransTable :: [Transition] -> M.Map (State, Condition) State
makeTransTable = M.fromList . map (\(from, ch, to) -> ((from, ch), to))

-- States in Purple Dragon 3-36
sA = stateTest [1, 2, 3]
sB = stateTest [1, 2, 3, 4]
sC = stateTest [1, 2, 3, 5]
sD = stateTest [1, 2, 3, 6]
sE = stateTest [1, 2, 3, 7]

-- Transitions
tAa = (sA, 'a', sB)
tAb = (sA, 'b', sC)
tBa = (sB, 'a', sB)
tBb = (sB, 'b', sD)
tCa = (sC, 'a', sB)
tCb = (sC, 'b', sC)
tDa = (sD, 'a', sB)
tDb = (sD, 'b', sE)
tEa = (sE, 'a', sB)
tEb = (sE, 'b', sC)

-- Prepare DFA invariants
initial :: State
initial = sA

accepts :: S.Set State
accepts = S.fromList [sE]

allStates :: S.Set State
allStates = S.fromList [sA, sB, sC, sD, sE]

transitions :: S.Set Transition
transitions = S.fromList [tAa, tAb, tBa, tBb, tCa, tCb, tDa, tDb, tEa, tEb]

charset :: S.Set Char
charset = S.fromList ['a', 'b']

dfa :: DFA
dfa = DFA {
    alphabet = charset,
    states = allStates,
    initialState = initial,
    acceptStates = accepts,
    transTable = makeTransTable $ S.toList transitions
}

-- Main spec
spec :: Spec
spec = do
    describe "DFA Build" $
        it "should be equal to hard-coded dfa" $
            build (transitions, initial, accepts) `shouldBe` dfa

    describe "DFA Transition" $ do
        it "B + 'b' -> D" $
            trans dfa sB 'b' `shouldBe` sD
        it "E + 'b' -> C" $
            trans dfa sE 'b' `shouldBe` sC
        it "A + 'c' -> Empty" $
            trans dfa sA 'c' `shouldBe` Empty
        it "Empty + 'a' -> Empty" $
            trans dfa Empty 'a' `shouldBe` Empty

    describe "DFA Running (a|b)*abb" $ do
        it "should only accept E" $
            accept dfa <$> [sA, sB, sC, sD, sE] `shouldBe` [False, False, False, False, True]
        it "should accept \"abaabb\"" $
            run dfa "abaabb" `shouldBe` ["test"]
        it "should not accept \"abaab\"" $
            run dfa "abaab" `shouldBe` []
        it "should accept \"abbaab\"" $
            run dfa "abbaab" `shouldBe` ["test"]

    describe "DFA Mapping States" $ do
        it "should stay the same when mapped by id" $
            mapStates id dfa `shouldBe` dfa
        it "should be empty when mapped by const Empty" $
            mapStates (const Empty) dfa `shouldBe` empty
        it "should stay the same when applied on empty dfa" $
            mapStates (\s@State {code} -> s { code = S.insert (-1) code }) empty `shouldBe` empty
        it "should delete the state and related stuff when set to Empty" $
            mapStates (\s -> if s /= sB then s else Empty) dfa `shouldBe` dfaDelB

    describe "DFA Making Index" $ do
        it "all states' code should be ascending indices from offset" $
            [code s | s <- S.toList . states $ makeIndex dfa 1] `shouldBe` S.singleton <$> [1..5]
        it "should stay the same when applied on empty dfa" $
            makeIndex empty 0 `shouldBe` empty
        it "should be equal to hard-coded dfa" $
            makeIndex dfa 0 `shouldBe` indexDFA

    describe "DFA Union - State and Property" $ do
        it "should stay the same when union with Empty state" $
            S.map (Empty <>) allStates `shouldBe` allStates
        it "A <> X -> State [1, 2, 3, 4] [\"test\", \"union\"]" $
            sA <> sX `shouldBe` state [1, 2, 3, 4] ["test", "union"]
        it "B <> Y -> State [1, 2, 3, 4] [\"test\"]" $
            sB <> sY `shouldBe` state [1, 2, 3, 4] ["test"]
        it "A <> Z -> State [1, 2, 3, 4, 5, 6] [\"test\", \"union\", \"more\"]" $
            sA <> sZ `shouldBe` state [1, 2, 3, 4, 5, 6] ["test", "union", "more"]
        it "should stay the same when union with empty dfa" $
            makeIndex (dfa <> empty) 0 `shouldBe` makeIndex dfa 0

    describe "DFA Union - Equal to Hard-Code" $ do
        it "Equality: initialState" $
            initialState (dfa <> dfa') `shouldBe` initialState unionDFA
        it "Equality: acceptStates" $
            acceptStates (dfa <> dfa') `shouldBe` acceptStates unionDFA
        it "Equality: alphabet" $
            alphabet (dfa <> dfa') `shouldBe` alphabet unionDFA
        it "Equality: states" $
            states (dfa <> dfa') `shouldBe` states unionDFA
        it "Equality: transTable" $
            transTable (dfa <> dfa') `shouldBe` transTable unionDFA
        it "Equality: ensembled DFA" $
            dfa <> dfa' `shouldBe` unionDFA

    describe "DFA Union - ((a|b)*abb)|(1|(ε|(a|11)a)((a|11)a)*(1|ε|(a|11)a)|ε|(a|11)a)" $ do
        it "should accept \"abaabb\" to [\"test\"]" $
            run unionDFA "abaabb" `shouldBe` ["test"]
        it "should accept \"aaaa\" to [\"test\", \"union\"]" $
            run unionDFA "aaaa" `shouldBe` ["test", "union"]
        it "should accept \"11aaa1\" to [\"union\", \"more\"]" $
            run unionDFA "11aaa1" `shouldBe` ["union", "more"]
        it "should not accept \"aaaaa\"" $
            test unionDFA "aaaaa" `shouldBe` False
        it "should accept \"\" to [\"test\", \"union\"]" $
            run unionDFA "" `shouldBe` ["test", "union"]

    describe "DFA Reduction" $ do
        it "should even delete the accept state when applied on dfa with B deleted" $
            reduction dfaDelB `shouldBe` dfaDelB'
        it "should be equal to hard-coded reduced union dfa" $
            reduction unionDFA `shouldBe` unionDFA'


{- ------- Stuff for DFA makeIndex ------- -}
sA' = sA { code = S.singleton 0 };
sB' = sB { code = S.singleton 1 };
sC' = sC { code = S.singleton 2 };
sD' = sD { code = S.singleton 3 };
sE' = sE { code = S.singleton 4 };

indexDFA = DFA {
    alphabet = charset,
    states = S.fromList [sA', sB', sC', sD', sE'],
    initialState = sA',
    acceptStates = S.fromList [sE'],
    transTable = makeTransTable [
        (sA', 'a', sB'), (sA', 'b', sC'),
        (sB', 'a', sB'), (sB', 'b', sD'),
        (sC', 'a', sB'), (sC', 'b', sC'),
        (sD', 'a', sB'), (sD', 'b', sE'),
        (sE', 'a', sB'), (sE', 'b', sC')
    ]
}

{- ------- Stuff for DFA union ------- -}
sX = state [2, 3, 4] ["union"]
sY = state [1, 2, 3] []
sZ = state [4, 5, 6] ["union", "more"]

-- Lacks two transition whose to_state is Empty
tXa = (sX, 'a', sY)
tX1 = (sX, '1', sZ)
tYa = (sY, 'a', sX)
tZ1 = (sZ, '1', sY)

-- Stands for regex
dfa' = build (S.fromList [tXa, tX1, tYa, tZ1], sX, S.fromList [sX, sZ])

-- Hard coded invariants
uAX = state [0, 6] ["test", "union"]
uAY = state [0, 5] ["test"]
uAZ = state [0, 7] ["test", "union", "more"]
uBX = state [1, 6] ["test", "union"]
uBY = state [1, 5] ["test"]
uBZ = state [1, 7] ["test", "union", "more"]
uCX = state [2, 6] ["test", "union"]
uCY = state [2, 5] ["test"]
uCZ = state [2, 7] ["test", "union", "more"]
uDX = state [3, 6] ["test", "union"]
uDY = state [3, 5] ["test"]
uDZ = state [3, 7] ["test", "union", "more"]
uEX = state [4, 6] ["test", "union"]
uEY = state [4, 5] ["test"]
uEZ = state [4, 7] ["test", "union", "more"]
uA0 = sA'; uB0 = sB'; uC0 = sC'; uD0 = sD'; uE0 = sE';
u0X = sX { code = S.singleton 6 };
u0Y = sY { code = S.singleton 5 };
u0Z = sZ { code = S.singleton 7 };
{- u00 = Empty -} -- Not used

unionDFA = DFA {
    alphabet = S.fromList ['a', 'b', '1'],
    states = S.fromList [uAX, uAY, uAZ, uBX, uBY, uBZ, uCX, uCY, uCZ, uDX, uDY, uDZ, uEX, uEY, uEZ, uA0, uB0, uC0, uD0, uE0, u0X, u0Y, u0Z],
    initialState = uAX,
    acceptStates = S.fromList [uAX, uAZ, uBX, uBZ, uCX, uCZ, uDX, uDZ, uEX, uEY, uEZ, uE0, u0X, u0Z],
    transTable = makeTransTable [
        -- Dual State Transitions
        (uAX, 'a', uBY), (uAX, 'b', uC0), (uAX, '1', u0Z),
        (uAY, 'a', uBX), (uAY, 'b', uC0), {-uAY, 1, u00,-}
        (uAZ, 'a', uB0), (uAZ, 'b', uC0), (uAZ, '1', u0Y),
        (uBX, 'a', uBY), (uBX, 'b', uD0), (uBX, '1', u0Z),
        (uBY, 'a', uBX), (uBY, 'b', uD0), {-uBY, 1, u00,-}
        (uBZ, 'a', uB0), (uBZ, 'b', uD0), (uBZ, '1', u0Y),
        (uCX, 'a', uBY), (uCX, 'b', uC0), (uCX, '1', u0Z),
        (uCY, 'a', uBX), (uCY, 'b', uC0), {-uCY, 1, u00,-}
        (uCZ, 'a', uB0), (uCZ, 'b', uC0), (uCZ, '1', u0Y),
        (uDX, 'a', uBY), (uDX, 'b', uE0), (uDX, '1', u0Z),
        (uDY, 'a', uBX), (uDY, 'b', uE0), {-uDY, 1, u00,-}
        (uDZ, 'a', uB0), (uDZ, 'b', uE0), (uDZ, '1', u0Y),
        (uEX, 'a', uBY), (uEX, 'b', uC0), (uEX, '1', u0Z),
        (uEY, 'a', uBX), (uEY, 'b', uC0), {-uEY, 1, u00,-}
        (uEZ, 'a', uB0), (uEZ, 'b', uC0), (uEZ, '1', u0Y),
        -- Single State Transtions
        (uA0, 'a', uB0), (uA0, 'b', uC0), {-uA0, 1, u00,-}
        (uB0, 'a', uB0), (uB0, 'b', uD0), {-uB0, 1, u00,-}
        (uC0, 'a', uB0), (uC0, 'b', uC0), {-uC0, 1, u00,-}
        (uD0, 'a', uB0), (uD0, 'b', uE0), {-uD0, 1, u00,-}
        (uE0, 'a', uB0), (uE0, 'b', uC0), {-uE0, 1, u00,-}
        (u0X, 'a', u0Y), {-u0X, b, u00,-} (u0X, '1', u0Z),
        (u0Y, 'a', u0X), {-u0Y, b, u00,-} {-u0Y, 1, u00,-}
        {-u0Z, a, u00,-} {-u0Z, b, u00,-} (u0Z, '1', u0Y)
    ]
}

{- ------- Stuff for DFA reduction ------- -}

dfaDelB = DFA {
    alphabet = S.fromList ['b'],
    states = S.fromList [sA, sC, sD, sE],
    initialState = sA,
    acceptStates = S.fromList [sE],
    transTable = makeTransTable $ [tAb, tCb, tDb, tEb]
}

-- Reduction on dfa
dfaDelB' = DFA {
    alphabet = S.fromList ['b'],
    states = S.fromList [sA, sC],
    initialState = sA,
    acceptStates = S.empty,
    transTable = makeTransTable $ [tAb, tCb]
}

-- Reduction on unioned dfa
unionDFA' = DFA {
    alphabet = S.fromList ['a', 'b', '1'],
    states = S.fromList [uAX, uBX, uBY, uB0, uC0, uD0, uE0, u0X, u0Y, u0Z],
    initialState = uAX,
    acceptStates = S.fromList [uAX, uBX, uE0, u0X, u0Z],
    transTable = makeTransTable [
        -- Dual State Transitions
        (uAX, 'a', uBY), (uAX, 'b', uC0), (uAX, '1', u0Z),
        {-uAY, a, uBX,-} {-uAY, b, uC0,-} {-uAY, 1, u00,-}
        {-uAZ, a, uB0,-} {-uAZ, b, uC0,-} {-uAZ, 1, u0Y,-}
        (uBX, 'a', uBY), (uBX, 'b', uD0), (uBX, '1', u0Z),
        (uBY, 'a', uBX), (uBY, 'b', uD0), {-uBY, 1, u00,-}
        {-uAZ, a, uB0,-} {-uAZ, b, uD0,-} {-uAZ, 1, u0Y,-}
        {-uAX, a, uBY,-} {-uAX, b, uC0,-} {-uAX, 1, u0Z,-}
        {-uAY, a, uBX,-} {-uAY, b, uC0,-} {-uCY, 1, u00,-}
        {-uAZ, a, uB0,-} {-uAZ, b, uC0,-} {-uAZ, 1, u0Y,-}
        {-uAX, a, uBY,-} {-uAX, b, uE0,-} {-uAX, 1, u0Z,-}
        {-uAY, a, uBX,-} {-uAY, b, uE0,-} {-uDY, 1, u00,-}
        {-uAZ, a, uB0,-} {-uAZ, b, uE0,-} {-uAZ, 1, u0Y,-}
        {-uAX, a, uBY,-} {-uAX, b, uC0,-} {-uAX, 1, u0Z,-}
        {-uAY, a, uBX,-} {-uAY, b, uC0,-} {-uEY, 1, u00,-}
        {-uAZ, a, uB0,-} {-uAZ, b, uC0,-} {-uAZ, 1, u0Y,-}
        -- Single State Transtions
        {-uA0, a, uB0,-} {-uA0, b, uC0,-} {-uA0, 1, u00,-}
        (uB0, 'a', uB0), (uB0, 'b', uD0), {-uB0, 1, u00,-}
        (uC0, 'a', uB0), (uC0, 'b', uC0), {-uC0, 1, u00,-}
        (uD0, 'a', uB0), (uD0, 'b', uE0), {-uD0, 1, u00,-}
        (uE0, 'a', uB0), (uE0, 'b', uC0), {-uE0, 1, u00,-}
        (u0X, 'a', u0Y), {-u0X, b, u00,-} (u0X, '1', u0Z),
        (u0Y, 'a', u0X), {-u0Y, b, u00,-} {-u0Y, 1, u00,-}
        {-u0Z, a, u00,-} {-u0Z, b, u00,-} (u0Z, '1', u0Y)
    ]
}
