{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For MonadState s (IRBuilderT m) instance
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module Language.C.Program where

import Language.C.Syntax
import Data.Map
import Data.Functor.Foldable

import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fail

{- Program type definition -}

data SymbolTable = SymbolTable {
    symbols :: Map Identifier (Int, Declaration), -- <id, stmtNo, declaration>
    blocks :: Map Int SymbolTable
}

data ProgramState = ProgramState {
    compilationUnit :: CompilationUnit,
    symbolTable :: SymbolTable
}

newtype ProgramT m a = ProgramT { unProgram :: StateT ProgramState m a } deriving (
    Functor, Alternative, Applicative, Monad, MonadCont,
    MonadError e, MonadFix, MonadIO, MonadPlus, MonadTrans, MonadFail)

type Program = ProgramT Identity

class Monad m => MonadProgram m where
    liftProgramState :: State ProgramState a -> m a
    default liftProgramState :: (MonadTrans t, MonadProgram m1, m ~ t m1)
                              => State ProgramState a -> m a
    liftProgramState = lift . liftProgramState

instance Monad m => MonadProgram (ProgramT m) where
    liftProgramState (StateT s) = ProgramT $ StateT $ pure . runIdentity . s

instance MonadState s m => MonadState s (ProgramT m) where
  state = lift . state

{- Program related operations -}

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable mempty mempty
