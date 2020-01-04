module Language.C.Syntax.Parsec.Parser where

import Language.C.Syntax
import Language.C.Syntax.Utils (TypeInfo, computeType)
import Language.C.Syntax.Parsec.Lexer
import Language.C.Semantic (constEval)
import Data.Functor
import Data.Functor.Foldable
import Data.Bifunctor
import Data.Composition
import Control.Monad

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

-- <translation-unit> ::= {<external-declaration>}*
compilationUnit :: Parser CompilationUnit
compilationUnit = many externalDeclaration

-- <external-declaration> ::= <function-definition>
--                          | <declaration>
externalDeclaration :: Parser ExternalDeclaration
externalDeclaration = functionDefinition <|> convDeclaration <$> declaration where
    convDeclaration _ = _

-- <function-definition> ::= {<declaration-specifier>}* <declarator> {<declaration>}* <compound-statement>
functionDefinition :: Parser ExternalDeclaration
functionDefinition = do
    specifiers             <- many declarationSpecifier
    (modifier, identifier) <- declarator
    _                      <- many declaration -- Don't know why there are declarations here
    let Function retType args = computeType (Left modifier : specifiers)
    FunctionDef retType identifier args <$> compoundStatement

-- <declaration> ::= {<declaration-specifier>}+ {<init-declarator>} ;
declaration :: Parser [Declaration]
declaration = do
    specifiers <- many1 declarationSpecifier
    let genDeclaration ((modifier, identifier), init) = Declaration idType (Just identifier) init where
            idType = computeType (Left modifier : specifiers)
    map genDeclaration <$> commaSep initDeclarator

-- <declaration-specifier> ::= <storage-class-specifier>
--                           | <type-specifier>
--                           | <type-qualifier>
declarationSpecifier :: Parser TypeInfo
declarationSpecifier = typeSpecifier
                   <|> Left <$> typeQualifier
                -- <|> storageClassSpecifier

-- <type-specifier> ::= <primitive-type>
--                    | <struct-or-union-specifier>
--                    | <enum-specifier>
--                    | <typedef-name>
typeSpecifier :: Parser TypeInfo
typeSpecifier = primitiveType
            -- <|> structOrUnionSpecifier
            -- <|> enumSpecifier
            -- <|> typedefName

-- <type-qualifier> ::= const
--                    | volatile
typeQualifier :: Parser (Type -> Type)
typeQualifier = reserved "const" $> Const
            -- <|> reserved "volatile" $> Volatile

-- <pointer> ::= * {<type-qualifier>}* {<pointer>}?
pointer :: Parser (Type -> Type)
pointer = do
    reservedOp "*"
    qualifiers <- many typeQualifier
    outerPtr   <- pointer
    return $ foldr1 (.) (outerPtr : qualifiers ++ [Pointer])

-- <init-declarator> ::= <declarator>
--                     | <declarator> = <initializer>
initDeclarator :: Parser ((Type -> Type, Identifier), Statement)
initDeclarator = (,) <$> declarator <*> option emptyStmt (reservedOp "=" >> initializer)

-- <initializer> ::= <assignment-expression>
--                 | { <initializer-list> }
--                 | { <initializer-list> , }
initializer :: Parser Statement
initializer = exprStmt <$> assignmentExpression
          <|> braces initializerList

-- <initializer-list> ::= <initializer>
--                      | <initializer-list> , <initializer>
initializerList :: Parser Statement
initializerList = compound <$> commaSep initializer

-- <declarator> ::= {<pointer>}? <direct-declarator>
declarator :: Parser (Type -> Type, Identifier)
declarator = do
    ptrSpecifier <- option id pointer
    (specifier, identifier) <- directDeclarator
    return (specifier . ptrSpecifier, identifier)

-- <direct-declarator> ::= <identifier>
--                       | ( <declarator> )
--                       | <direct-declarator> [ {<constant-expression>}? ]
--                       | <direct-declarator> ( <parameter-type-list> )
--                       | <direct-declarator> ( {<identifier>}* ) -- obsolete-style declarator
directDeclarator :: Parser (Type -> Type, Identifier)
directDeclarator = base `unaryChainl1` rest where
    base = (id, ) <$> identifier <|> parens declarator
    rest = do
        modifier <- arrayDeclModifier <|> functionDeclModifier
        return $ first (modifier .) -- (modifier . specifier, identifier)

-- <array-decl-modifier> := [<expression-type>]
arrayDeclModifier :: Parser (Type -> Type)
arrayDeclModifier = do
    Fix (Literal _ (IntegerLit n)) <- constEval <$> brackets constantExpression
    return $ flip Array n

-- <function-decl-modifier> := (<parameter-type>)
functionDeclModifier :: Parser (Type -> Type)
functionDeclModifier = do
    params <- braces parameterTypeList
    return $ flip Function params

-- <parameter-type-list> ::= <parameter-list>
--                         | <parameter-list> , ... -- varArg
parameterTypeList :: Parser [(Type, Maybe Identifier)]
parameterTypeList = parameterList <* optional (comma <* reservedOp "...") -- ignore vararg

-- <parameter-list> ::= <parameter-declaration>
--                    | <parameter-list> , <parameter-declaration>
parameterList :: Parser [(Type, Maybe Identifier)]
parameterList = commaSep parameterDeclaration

-- <parameter-declaration> ::= {<declaration-specifier>}+ <declarator>
--                           | {<declaration-specifier>}+ <abstract-declarator>
--                           | {<declaration-specifier>}+
parameterDeclaration :: Parser (Type, Maybe Identifier)
parameterDeclaration = do
    specifiers <- many1 declarationSpecifier
    (modifier, identifier) <- option (id, Nothing) (
            second Just <$> declarator
        <|> (, Nothing) <$> abstractDeclarator)
    return (computeType (Left modifier : specifiers), identifier)

-- <abstract-declarator> ::= <pointer>
--                         | <pointer> <direct-abstract-declarator>
--                         | <direct-abstract-declarator>
abstractDeclarator :: Parser (Type -> Type)
abstractDeclarator = do
    specifier <- option id pointer
    modifier  <- option id directAbstractDeclarator
    return (modifier . specifier)

-- <direct-abstract-declarator> ::=  ( <abstract-declarator> )
--                                | {<direct-abstract-declarator>}? [ {<constant-expression>}? ]
--                                | {<direct-abstract-declarator>}? ( {<parameter-type-list>}? )
directAbstractDeclarator :: Parser (Type -> Type)
directAbstractDeclarator = base `unaryChainl1` rest where
    base = parens abstractDeclarator <|> arrayDeclModifier <|> functionDeclModifier
    rest = do
        modifier <- arrayDeclModifier <|> functionDeclModifier
        return (modifier .)

-- <type-name> ::= {<specifier-qualifier>}+ {<abstract-declarator>}?
typeName :: Parser Type
typeName = do
    specifiers <- many1 (typeSpecifier <|> Left <$> typeQualifier)
    modifier <- option id abstractDeclarator
    return $ computeType (Left modifier : specifiers)

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

-- <statement> ::= <leabeled-statement>
--               | <expression-statement>
--               | <compound-statement>
--               | <selection-statement>
--               | <iteration-statement>
--               | <jump-statement>
statement :: Parser Statement
statement = compoundStatement
        <|> expressionStatement
        <|> declarationStatement
        <|> labeledStatement
        <|> selectionStatement
        <|> iterationStatement
        <|> jumpStatement

-- <compound-statement> ::= { {<declaration>}* {<statement>}* }
compoundStatement :: Parser Statement
compoundStatement = compound <$> braces (many statement)

-- <labeled-statement> ::= <identifier> : <statement>
--                       | case <constant-expression> : <statement>
--                       | default : <statement>
labeledStatement :: Parser Statement
labeledStatement = identifier >> colon >> statement
            --    <|> (reserved "case" >> constantExpression >> colon >> statement)
            --    <|> (reserved "default" >> statement)

-- <expression-statement> ::= {<expression>}? ;?
expressionStatement :: Parser Statement
expressionStatement = option emptyStmt (exprStmt <$> expression) <* optionMaybe semi

-- <expression-statement> ::= <declaration>
declarationStatement :: Parser Statement
declarationStatement = Fix . LocalVar <$> declaration

-- <selection-statement> ::= if ( <expression> ) <statement>
--                         | if ( <expression> ) <statement> else <statement>
--                         | switch ( <expression> ) <statement>
selectionStatement :: Parser Statement
selectionStatement = ifStatement <*> parens expressionStatement <*> statement <*> elseStatement statement

-- <iteration-statement> ::= while ( <expression> ) <statement>
--                         | do <statement> while ( <expression> ) ;
--                         | for ( {<expression>}? ; {<expression>}? ; {<expression>}? ) <statement>
iterationStatement :: Parser Statement
iterationStatement = whileStatement <*> parens expressionStatement <*> statement
                 <|> doWhileStatement <*> statement <* reserved "while" <*> parens expressionStatement
                 <|> forStatement <*> parens ((,,) <$> expressionStatement <*> expressionStatement <*> expressionStatement) <*> statement

-- <jump-statement> ::= goto <identifier> ;
--                    | continue ;
--                    | break ;
--                    | return {<expression>}? ;
jumpStatement :: Parser Statement
jumpStatement = returnStatement <*> expressionStatement <* semi
            <|> reserved "continue" $> Fix Continue <* semi
            <|> reserved "break" $> Fix Break <* semi

{- Utils -}

unaryChainl1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a) -> ParsecT s u m a
unaryChainl1 p op = rest =<< p where
    rest x = do f <- op
                rest (f x)
          <|> return x
