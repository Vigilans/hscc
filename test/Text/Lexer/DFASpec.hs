module Text.Lexer.DFASpec where

import Test.Hspec
import Test.QuickCheck
import Text.Lexer.DFA
import qualified Data.Set as S
import qualified Data.Map as M

-- Helper state function
state :: [Int] -> State
state code = State { code = S.fromList code, tags = ["test"] }

-- States in Purple Dragon 3-36
sA = state [1, 2, 3]
sB = state [1, 2, 3, 4]
sC = state [1, 2, 3, 5]
sD = state [1, 2, 3, 6]
sE = state [1, 2, 3, 7]

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

transitions :: [Transition]
transitions = [tAa, tAb, tBa, tBb, tCa, tCb, tDa, tDb, tEa, tEb]

charset :: S.Set Char
charset = S.fromList ['a', 'b']

dfa :: DFA
dfa = DFA {
    alphabet = charset,
    states = allStates,
    initialState = initial,
    acceptStates = accepts,
    transTable = M.fromList $ map (\(from, ch, to) -> ((from, ch), to)) transitions
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

    describe "DFA Making Index" $ do
        it "all states' code should be ascending indices from offset" $
            [code s | s <- S.toList . states $ makeIndex dfa 1] `shouldBe` S.singleton <$> [1..5]
        it "should stay the same when applied on empty dfa" $
            makeIndex empty 0 `shouldBe` empty

    describe "DFA Union" $ do
        it "should stay the same when union with Empty state" $
            S.map (Empty <>) allStates `shouldBe` allStates
        it "A <> X -> State [1, 2, 3, 4] [\"test\", \"union\"]" $
            sA <> sX `shouldBe` stateT [1, 2, 3, 4] ["test", "union"]
        it "B <> Y -> State [1, 2, 3, 4] [\"test\"]" $
            sB <> sY `shouldBe` stateT [1, 2, 3, 4] ["test"]
        it "A <> Z -> State [1, 2, 3, 4, 5, 6] [\"test\", \"union\", \"more\"]" $
            sA <> sZ `shouldBe` stateT [1, 2, 3, 4, 5, 6] ["test", "union", "more"]

-- Stuff for DFA union
stateT :: [Int] -> [String] -> State
stateT code tags = State { code = S.fromList code, tags }

sX = stateT [2, 3, 4] ["union"]
sY = stateT [1, 2, 3] []
sZ = stateT [4, 5, 6] ["union", "more"]
