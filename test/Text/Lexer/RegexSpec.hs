module Text.Lexer.RegexSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Lexer.Regex

spec :: Spec
spec = describe "Read and Show" $ do
    it "reads (c|d|e)" $
        readRegex "c|d|e" `shouldBe` Union (Union (Symbol 'c') (Symbol 'd')) (Symbol 'e')

    it "shows (c|d|e)" $
        showRegex (readRegex "c|d|e") `shouldBe` "((c|d)|e)"
