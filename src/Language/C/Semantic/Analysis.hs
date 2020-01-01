module Language.C.Semantic.Analysis where

import Language.C.Program
import Language.C.Syntax
import Language.C.Syntax.Utils (bool, int)
import Data.Bifunctor
import Data.Functor.Foldable

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Zip

newtype AnalysisState = AnalysisState {
    countStack :: [Int] -- The order of current statement in current block
}

newtype AnalysisT m a = AnalysisT { unAnalysis :: StateT AnalysisState m a } deriving (
    Functor, Alternative, Applicative, Monad, MonadCont,
    MonadError e, MonadFix, MonadPlus, MonadIO, MonadTrans, MonadFail)

class Monad m => MonadAnalysis m where
    liftAnalysisState :: State AnalysisState a -> m a
    default liftAnalysisState :: (MonadTrans t, MonadAnalysis m1, m ~ t m1)
                              => State AnalysisState a -> m a
    liftAnalysisState = lift . liftAnalysisState

instance Monad m => MonadAnalysis (AnalysisT m) where
    liftAnalysisState (StateT s) = AnalysisT $ StateT $ pure . runIdentity . s

instance MonadProgram m => MonadProgram (AnalysisT m)

instance MonadState s m => MonadState s (AnalysisT m) where
    state = lift . state

type ProgramAnalysisT m = AnalysisT (ProgramT m)

type ProgramAnalysis = ProgramAnalysisT Identity
