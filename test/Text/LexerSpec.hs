-- module Text.LexerSpec where

-- import Test.Hspec
-- import Test.QuickCheck

-- lexFileInput = "  \n\t\n\
--     \%{\n\n\
--     \#include <stdio.h>\n\
--     \aaa\n\
--     \%}\n\
--     \D\t[0-9]\n\
--     \A\t[1-3]*\n\
--     \%%\n\
--     \auto\t{ printf(\"AUTO\"); }\n\
--     \case\t{ printf(\"CASE\"); }\n\
--     \{D}\t{ printf(\"IDENTIFIER\"); }\n\
--     \%%\n\
--     \main() {\n\
--     \\tyylex();\n\
--     \}"

-- runLexer = case runParser parseLexer (Lexer "" mempty mempty empty "") "" lexFileInput of
--     Left err -> error $ show err
--     Right lexer -> lexer
