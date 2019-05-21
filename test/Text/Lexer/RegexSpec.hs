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
        it "[a-cA-C ] is ((((((a|b)|c)|A)|B)|C)| )" $
            readAndShow "[a-cA-C ]" `shouldBe` "((((((a|b)|c)|A)|B)|C)| )"
        it "[\\\\\\t\\+\"] is (((\\|\t)|+)|\")" $ 
            readAndShow "[\\\\\\t\\+\"]" `shouldBe` "(((\\|\t)|+)|\")"
        it "[\\[\\]] is ([|])" $ 
            readAndShow "[\\[\\]]" `shouldBe` "([|])"
        it "[aaaa] is a" $
            readAndShow "[aaaa]" `shouldBe` "a"
        it "a\\t\\* is a\t*" $
            readAndShow "a\\t\\*" `shouldBe` "a\t*"
        it "a\\t\\**\\\\+ is a\t(*)*\\(\\)*" $
            readAndShow "a\\t\\**\\\\+" `shouldBe` "a\t(*)*\\(\\)*"
        -- TODO: negSet has bug here
        -- TODO: remove the second '-' below and see what will happen
        -- TODO: it may also not handle meta or escape chars correctly
        it "[^A-Za-z0-9!#%',/:;<=>{}&_-~\\|\\.\\*\\+\\?\\)\\(-] is (((((((((\\|\")|[)|^)|])|\n)|\t)|\v)|\f)| )" $
            readAndShow "[^A-Za-z0-9!#%',/:;<=>{}&_-~\\|\\.\\*\\+\\?\\)\\(-]"`shouldBe` "(((((((((\\|\")|[)|^)|])|\n)|\t)|\v)|\f)| )"