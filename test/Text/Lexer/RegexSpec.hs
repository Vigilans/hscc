module Text.Lexer.RegexSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Lexer.Regex

readAndShow :: String -> String
readAndShow = showRegex . readRegex

spec :: Spec
spec = do
    describe "Read and Show" $ do
        it "\"c|d|e\" is Union (Union (Symbol 'c') (Symbol 'd')) (Symbol 'e')" $
            readRegex "c|d|e" `shouldBe` Union (Union (Symbol 'c') (Symbol 'd')) (Symbol 'e')
        it "Union (Union (Symbol 'c') (Symbol 'd')) (Symbol 'e') is \"((c|d)|e)\"" $
            showRegex (Union (Union (Symbol 'c') (Symbol 'd')) (Symbol 'e')) `shouldBe` "((c|d)|e)"

    describe "Regex Parser" $ do
        it "a+ is a(a)*" $
            readAndShow "a+" `shouldBe` "a(a)*"
        it "[a-c] is ((a|b)|c)" $
            readAndShow "[a-c]" `shouldBe` "((a|b)|c)"
