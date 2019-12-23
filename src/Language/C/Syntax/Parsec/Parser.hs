module Language.C.Syntax.Parsec.Parser where

import Language.C.Syntax
import Language.C.Syntax.Utils (TypeInfo, computeType)
import Language.C.Syntax.Parsec.Lexer
import Data.Functor
import Data.Functor.Foldable
import Data.Bifunctor
import Data.Composition
import Control.Monad

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

-- <constant-expression> ::= <conditional-expression>
constantExpression :: Parser Expression
constantExpression = Fix . Constant <$> conditionalExpression

-- <conditional-expression> ::= <logical-or-expression>
--                            | <logical-or-expression> ? <expression> : <conditional-expression>
conditionalExpression :: Parser Expression
conditionalExpression = logicalOrExpression
                    <|> Fix <$> (Cond <$> logicalOrExpression <* reservedOp "?" <*> expression <* reservedOp ":" <*> conditionalExpression)

-- <logical-or-expression> ::= <logical-and-expression>
--                           | <logical-or-expression> || <logical-and-expression>
logicalOrExpression :: Parser Expression
logicalOrExpression = logicalAndExpression `chainl1` binaryOperator ["||"]

-- <logical-and-expression> ::= <inclusive-or-expression>
--                            | <logical-and-expression> && <inclusive-or-expression>
logicalAndExpression :: Parser Expression
logicalAndExpression = inclusiveOrExpression `chainl1` binaryOperator ["&&"]

-- <inclusive-or-expression> ::= <exclusive-or-expression>
--                             | <inclusive-or-expression> | <exclusive-or-expression>
inclusiveOrExpression :: Parser Expression
inclusiveOrExpression = exclusiveOrExpression `chainl1` binaryOperator ["|"]

-- <exclusive-or-expression> ::= <and-expression>
--                             | <exclusive-or-expression> ^ <and-expression>
exclusiveOrExpression :: Parser Expression
exclusiveOrExpression = andExpression `chainl1` binaryOperator ["^"]

-- <and-expression> ::= <equality-expression>
--                    | <and-expression> & <equality-expression>
andExpression :: Parser Expression
andExpression = equalityExpression `chainl1` binaryOperator ["&"]

-- <equality-expression> ::= <relational-expression>
--                         | <equality-expression> == <relational-expression>
--                         | <equality-expression> != <relational-expression>
equalityExpression :: Parser Expression
equalityExpression = relationalExpression `chainl1` binaryOperator ["==", "!="]

-- <relational-expression> ::= <shift-expression>
--                           | <relational-expression> < <shift-expression>
--                           | <relational-expression> > <shift-expression>
--                           | <relational-expression> <= <shift-expression>
--                           | <relational-expression> >= <shift-expression>
relationalExpression :: Parser Expression
relationalExpression = shiftExpression `chainl1` binaryOperator ["<", ">", "<=", ">="]

-- <shift-expression> ::= <additive-expression>
--                      | <shift-expression> << <additive-expression>
--                      | <shift-expression> >> <additive-expression>\
shiftExpression :: Parser Expression
shiftExpression = additiveExpression `chainl1` binaryOperator ["<<", ">>"]

-- <additive-expression> ::= <multiplicative-expression>
--                         | <additive-expression> + <multiplicative-expression>
--                         | <additive-expression> - <multiplicative-expression>
additiveExpression :: Parser Expression
additiveExpression = multiplicativeExpression `chainl1` binaryOperator ["+", "-"]

-- <multiplicative-expression> ::= <cast-expression>
--                               | <multiplicative-expression> * <cast-expression>
--                               | <multiplicative-expression> / <cast-expression>
--                               | <multiplicative-expression> % <cast-expression>
multiplicativeExpression :: Parser Expression
multiplicativeExpression = castExpression `chainl1` binaryOperator ["*", "/", "%"]

-- <cast-expression> ::= <unary-expression>
--                     | ( <type-name> ) <cast-expression>
castExpression :: Parser Expression
castExpression = unaryExpression
             <|> Fix <$> (Cast <$> parens typeName <*> castExpression)

-- <unary-expression> ::= <postfix-expression>
--                      | ++ <unary-expression>
--                      | -- <unary-expression>
--                      | <unary-operator> <cast-expression>
--                      | sizeof <unary-expression>
--                      | sizeof <type-name>
unaryExpression :: Parser Expression
unaryExpression = postfixExpression
            --   <|> unaryOperator ["++", "--"] <*> unaryExpression
              <|> unaryOperator ["+", "-", "&", "*", "~", "!"] <*> castExpression
              <|> sizeofExpression <*> (Right <$> unaryExpression)
              <|> sizeofExpression <*> (Left <$> typeName)

-- <postfix-expression> ::= <primary-expression>
--                        | <postfix-expression> [ <expression> ]
--                        | <postfix-expression> ( {<assignment-expression>}* )
--                        | <postfix-expression> . <identifier>
--                        | <postfix-expression> -> <identifier>
--                        | <postfix-expression> ++
--                        | <postfix-expression> --
postfixExpression :: Parser Expression
postfixExpression = primaryExpression `unaryChainl1`
                  ( (Fix `compose2` flip Index) <$> brackets expression
                <|> (Fix `compose2` flip Call)  <$> braces (commaSep assignmentExpression)
                -- <|> flip <$> binaryOperator [".", "->"] <*> (Variable <$> identifier)
                -- <|> unaryOperator ["++", "--"]
                  )

-- <primary-expression> ::= <identifier>
--                        | <constant>
--                        | <string>
--                        | ( <expression> )
primaryExpression :: Parser Expression
primaryExpression = Fix . Literal Unknown . Variable <$> identifier
                <|> constant
                <|> parens expression

-- <expression> ::= <assignment-expression>
--                | <expression> , <assignment-expression>
expression :: Parser Expression
expression = assignmentExpression `chainl1` binaryOperator [","]

-- <assignment-expression> ::= <conditional-expression>
--                           | <unary-expression> <assignment-operator> <assignment-expression>
assignmentExpression :: Parser Expression
assignmentExpression = conditionalExpression
                   <|> (unaryExpression `chainr1` assignmentOperator ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="])

-- <constant> ::= <integer-constant>
--              | <character-constant>
--              | <floating-constant>
--              | <enumeration-constant>
constant :: Parser Expression
constant = Fix . uncurry Literal <$>
         ( integerConstant
       <|> characterConstant
       <|> floatingConstant
       <|> stringConstant)

{- Utils -}

unaryChainl1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a) -> ParsecT s u m a
unaryChainl1 p op = rest =<< p where
    rest x = do f <- op
                rest (f x)
          <|> return x
