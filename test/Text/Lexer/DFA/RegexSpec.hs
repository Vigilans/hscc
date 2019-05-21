module Text.Lexer.DFA.RegexSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Lexer.Regex
import Text.Lexer.DFA.Regex
import Text.Lexer.DFA
import qualified Data.Map as M
import qualified Data.Set.Monad as S

regex :: Regex
regex = readRegex "(a|b)*abb"

regex' :: Regex
regex'= augment regex

state :: [Int] -> [String] -> State
state c s = State { code = S.fromList c, tags = s }

sA = state [1,2,3]   []
sB = state [1,2,3,4] []
sC = state [1,2,3,5] []
sD = state [1,2,3,6] ["regex"]

transitions = S.fromList [
    (sA, 'a', sB), (sA, 'b', sA),
    (sB, 'a', sB), (sB, 'b', sC),
    (sC, 'a', sB), (sC, 'b', sD),
    (sD, 'a', sB), (sD, 'b', sA)
    ]

regexDFA :: DFA
regexDFA = build (transitions, sA, S.fromList [sD])

spec :: Spec
spec = do
    let (RegexAttr{ nullable, firstpos, lastpos }, RegexPose{ followpos, leafsymb }) = regexFunction regex'
    describe "Regex Functions" $ do
        it "augmented regex is ((a|b))*abb#" $
            showRegex regex' `shouldBe` "((a|b))*abb#"
        it "nullable, firstpos, lastpos are False, {1,2,3}, {6}" $
            (nullable, firstpos, lastpos) `shouldBe` (False, S.fromList [1,2,3], S.fromList [6])
        it "follow poses are [{1,2,3}, {1,2,3}, {4}, {5}, {6}, ∅]" $
            S.toList <$> M.elems followpos `shouldBe` [[1,2,3], [1,2,3], [4], [5], [6], []]
        it "leaf symbols are [a, b, a, b, b, #]" $
            showRegex <$> M.elems leafsymb `shouldBe` ["a", "b", "a", "b", "b", "#"]

    describe "Regex to DFA" $
        it "should be equal to hard-coded dfa" $
            regex2dfa regex "regex" `shouldBe` regexDFA
