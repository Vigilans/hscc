module Language.C.Syntax.Parsec.Lexer where

import Language.C.Syntax
import Language.C.Syntax.Utils (TypeInfo, stringType)
import Data.Functor
import Data.Functor.Foldable
import Data.Composition

import Control.Monad
import Control.Arrow

import Text.Parsec        as Parsec
import Text.Parsec.String as Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (GenLanguageDef(..), GenTokenParser(TokenParser))
import qualified Text.Parsec.Token as Parsec

minicDef :: Parsec.LanguageDef st
minicDef = emptyDef {
    commentStart    = "/*",
    commentEnd      = "*/",
    commentLine     = "//",
    nestedComments  = True,
    identStart      = char '_' <|> letter,
    identLetter     = char '_' <|> alphaNum,
    reservedNames   = [],
    reservedOpNames = [
        "+",  "-",  "*",  "/",  "%",  "&",  "|",  "^",  "<<",  ">>",
        "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "=",
        "==", "!=", "<=", ">=", "<",  ">",  "&&", "||",
        "~",  "!",  ".",  "->", "?",  ":"
    ],
    caseSensitive   = True
}

minicLexer :: Parsec.TokenParser st
minicLexer@TokenParser{..} = Parsec.makeTokenParser minicDef

-- Constant

integerConstant :: GenParser Char st (Typed Lit)
integerConstant = (Integer Int Signed, ) . IntegerLit <$> integer

characterConstant :: GenParser Char st (Typed Lit)
characterConstant = (Integer Char Signed, ) . CharLit <$> charLiteral

floatingConstant :: GenParser Char st (Typed Lit)
floatingConstant = (Floating Double, ) . FloatLit <$> float

stringConstant :: GenParser Char st (Typed Lit)
stringConstant = (stringType &&& StringLit) <$> stringLiteral

-- Type

primitiveType :: GenParser Char st TypeInfo
primitiveType = (reserved "void"   $> Right Void)
            <|> (reserved "char"   $> Right (Integer Char Unsigned))
            <|> (reserved "short"  $> Right (Integer Short Signed))
            <|> (reserved "int"    $> Right (Integer Int Signed))
            <|> (reserved "long"   $> Right (Integer Long Signed))
            <|> (reserved "float"  $> Right (Floating Float))
            <|> (reserved "double" $> Right (Floating Double))
            <|> (reserved "signed"   $> Left (turnSignedness Signed))
            <|> (reserved "unsigned" $> Left (turnSignedness Unsigned))

turnSignedness :: Signedness -> Type -> Type
turnSignedness s Unknown = Integer Int s
turnSignedness s (Integer t _) = Integer t s
turnSignedness s _ = error "Type is not of integer type"

-- Operator

primitiveOperator :: forall op st. (String -> op) -> [String] -> GenParser Char st op
primitiveOperator parse ops = choice (run <$> ops) where
    run op = do
        reservedOp op
        return $ parse op

unaryOperator :: [String] -> GenParser Char st (Expression -> Expression)
unaryOperator = primitiveOperator (Fix `compose2` parse) where
    parse "+" = Unary Pos
    parse "-" = Unary Neg

binaryOperator :: [String] -> GenParser Char st (Expression -> Expression -> Expression)
binaryOperator = primitiveOperator (Fix `compose3` parse) where
    parse "+" = Binary Add
    parse "-" = Binary Sub
    parse "*" = Binary Mul
    parse "/" = Binary Div
    parse "%" = Binary Mod

assignmentOperator :: [String] -> GenParser Char st (Expression -> Expression -> Expression)
assignmentOperator = primitiveOperator (Fix `compose3` parse) where
    parse "="  = Assign Nop
    parse "+=" = Assign Add
    parse "-=" = Assign Sub
    parse "*=" = Assign Mul
    parse "/=" = Assign Div
    parse "%=" = Assign Mod
