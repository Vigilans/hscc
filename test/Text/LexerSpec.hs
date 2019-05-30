module Text.LexerSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Lexer
import Text.ParserCombinators.Parsec

lexFileInput = "  \n\t\n\
    \%{\n\n\
    \#include <stdio.h>\n\
    \aaa\n\
    \%}\n\
    \D\t[0-9]\n\
    \A\t[1-3]*\n\
    \%%\n\
    \auto\t{ printf(\"AUTO\"); }\n\
    \case\t{ printf(\"CASE\"); }\n\
    \{D}\t{ printf(\"IDENTIFIER\"); }\n\
    \%%\n\
    \main() {\n\
    \\tyylex();\n\
    \}"

readLexer text = case runParser parseLexer (RawLexer "" mempty mempty mempty "") "" text of
    Left err -> error $ show err
    Right lexer -> lexer

spec :: Spec
spec = describe "Lexer" $ do
    it "should successfully parse sample lex input" $
        const True (readLexer lexFileInput) `shouldBe` True
    it "should successfully build lexer string" $
        const True (buildLexer ("", lexFileInput)) `shouldBe` True
