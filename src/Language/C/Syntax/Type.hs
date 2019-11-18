{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.C.Syntax.Type where

import Language.C.Syntax.Token
import Data.Word
import Data.Aeson (ToJSON)
import GHC.Generics

type Typed = ((,) Type)

data Type = Unknown -- Used as base type for type deduction
          | Void
          | Integer  IntType Signedness
          | Floating FloatType
          | Array    Type Integer
          | Pointer  Type
          | Const    Type
          | Function Type [Typed (Maybe Identifier)]
          | Struct [Typed Identifier]
          | Union  [Typed Identifier]
          | Typedef Identifier
          deriving (Eq, Show, Generic, ToJSON)

data IntType = Bool | Char | Short | Int | Long deriving (Eq, Ord, Show, Generic, ToJSON)

data FloatType = Half | Float | Double deriving (Eq, Ord, Show, Generic, ToJSON)

data Signedness = Unsigned | Signed deriving (Eq, Ord, Show, Generic, ToJSON) -- Signed will be elevated to unsigned

sizeof :: Type -> Integer
sizeof  Void             = 0
sizeof (Integer Bool  _) = 1
sizeof (Integer Char  _) = 1
sizeof (Integer Short _) = 2
sizeof (Integer Int   _) = 4
sizeof (Integer Long  _) = 4
sizeof (Floating Half)   = 2
sizeof (Floating Float)  = 4
sizeof (Floating Double) = 8
sizeof (Array t n)       = toInteger n * sizeof t
sizeof (Pointer t)       = 4
sizeof (Const t)         = sizeof t
sizeof f@(Function _ _)  = sizeof $ Pointer f
sizeof (Struct ts)       = sum $ sizeof . fst <$> ts
sizeof (Union  ts)       = maximum $ sizeof . fst <$> ts

commonType :: Type -> Type -> Type
commonType (Integer a sa) (Integer b sb)     = Integer  (max a b) (max sa sb)
commonType (Floating a)   (Floating b)       = Floating (max a b)
commonType (Floating a) (Integer b Unsigned) = Floating a
commonType (Floating a) (Integer b Signed)   = Floating a
commonType (Integer a Unsigned) (Floating b) = Floating b
commonType (Integer a Signed)   (Floating b) = Floating b
commonType _ _ = Unknown

elemType :: Type -> Type
elemType (Array t _) = t
elemType (Pointer t) = t
elemType _ = Unknown

returnType :: Type -> Type
returnType (Function t _) = t
returnType _ = Unknown

argTypes :: Type -> [Type]
argTypes (Function _ ts) = fst <$> ts
argTypes _ = pure Unknown
