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

data Statement
    = EmptyStmt
    | ExprStmt Expression -- Simple expression statement
    | Compound [Statement] -- Block statement
    | LocalVar [Declaration] -- Local variable declaration in one line
    | If Expression Statement Statement -- If cond then (else)
    | While Bool Expression Statement -- While isDoWhile cond then
    | For Statement Statement Statement Statement -- For init cond after body
    | Return (Maybe Expression) -- Return expr
    | Break -- Break loop
    | Continue -- Continue loop
    deriving (Eq, Show, Generic)

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
