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
