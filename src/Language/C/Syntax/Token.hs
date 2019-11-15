{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.C.Syntax.Token where

import Data.Word
import Data.Aeson (ToJSON)
import GHC.Generics

type Identifier = String

data Lit = IntegerLit Integer
         | CharLit Char
         | FloatLit Double
         | StringLit String
         | Variable Identifier
         deriving (Eq, Show, Generic, ToJSON)

data UnaryOp = Pos -- +
             | Neg -- -
             | Not -- !
             | Inv -- ~
             | Adr -- &
             | Der -- *
             deriving (Eq, Show, Generic, ToJSON)

data BinaryOp = Nop -- =
              | Add -- +
              | Sub -- -
              | Mul -- *
              | Div -- /
              | Mod -- %
              | Bnd -- &
              | Bor -- |
              | Xor -- ^
              | And -- &&
              | Or -- ||
              | Eq -- ==
              | Ne -- !=
              | Gt -- >
              | Ge -- >=
              | Lt -- <
              | Le -- <=
              | Com -- ,
              deriving (Eq, Show, Generic, ToJSON)
