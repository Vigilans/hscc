{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.C.Syntax.AST where

import Language.C.Syntax.Token
import Language.C.Syntax.Type
import Data.Word
import Data.Functor.Classes
import Data.Functor.Foldable
import GHC.Generics

type CompilationUnit = [ExternalDeclaration]

data ExternalDeclaration
    = FunctionDef    Type String [Typed (Maybe Identifier)] Statement
    | ExternFuncDecl Type String [Type]
    | GlobalVarDef   Declaration
    | ExternVarDecl  Declaration
    deriving (Eq, Show, Generic)

data Declaration = Declaration
    Type -- Variable type
    (Maybe Identifier) -- Variable name
    Statement -- Optional initializer
    deriving (Eq, Show, Generic)

data StatementF a
    = EmptyStmt
    | ExprStmt Expression -- Simple expression statement
    | LocalVar [Declaration] -- Local variable declaration in one line
    | Compound [a] -- Block statement
    | If a a (Maybe a) -- If cond then (maybe else)
    | While Bool a a -- While isDoWhile cond then
    | For a a a a -- For init cond after body
    | Return a -- Return (maybe expr)
    | Break -- Break loop
    | Continue -- Continue loop
    deriving (Eq1, Show1, Functor, Foldable, Traversable)

type Statement = Fix StatementF

data ExpressionF a
    = Unary  UnaryOp  a
    | Binary BinaryOp a a
    | Assign BinaryOp a a
    | Cond a a a
    | Call a [a]
    | Cast Type a
    | Index a a -- arr idx
    | Sizeof (Either Type a)
    | Constant a
    | Literal Type Lit
    deriving (Eq1, Show1, Functor, Foldable, Traversable)

type Expression = Fix ExpressionF

emptyStmt :: Statement
emptyStmt = Fix EmptyStmt

exprStmt :: Expression -> Statement
exprStmt = Fix . ExprStmt

compound :: [Statement] -> Statement
compound = Fix . Compound
