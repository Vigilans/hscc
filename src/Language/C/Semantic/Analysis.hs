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

{- Analysis launcher -}

-- analyseExtDecl :: (Monad m) => ExternalDeclaration -> ProgramAnalysis a
-- analyseExtDecl (FunctionDef _ _ _ body) = _

analyseStatement :: (MonadAnalysis m)
                 => (Statement -> m a)
                 -> (Statement -> m a)
analyseStatement _ = _
    -- s :: (Monad m, MonadAnalysis m) => StatementF Statement -> m ()
    -- s = lift . analyse
    -- analyse :: StatementF Statement -> ProgramAnalysis ()
    -- analyse (Compound stmts) = _

analyseExpression :: (MonadAnalysis m)
                  => (Type -> ExpressionF (Typed a) -> m a) -- Expression type will be passed as first argument
                  -> (Expression -> m (Typed a)) -- Expression will analysed with a type
analyseExpression alg = cataM (\e -> do { t <- exprType e; r <- alg t e; return (t, r) }) where
    exprType (Literal _ (Variable id)) = return Unknown
    exprType (Literal t lit)           = return t
    exprType (Constant (t, e))         = return t
    exprType (Sizeof (Right (t, _)))   = return int
    exprType (Sizeof (Left t))         = return int
    exprType (Cast t' (t, e))          = return t'
    exprType (Assign op (t, l) r)      = return t
    exprType (Unary op (t, e))         = return $ fst $ unaryFuncType op t
    exprType (Binary op (a, _) (b, _)) = return $ fst $ binaryFuncType op a b
    exprType (Cond (_, c) a b)         = return $ commonType (fst a) (fst b)
    exprType (Index (t, arr) (_, idx)) = return $ elemType t

unaryFuncType :: UnaryOp -> Type -> (Type, Type) -- (retType, argType)
unaryFuncType Pos t = (t, t)
unaryFuncType Neg t = (t, t)
unaryFuncType Not t = (bool, bool)
unaryFuncType Inv t = (t, t)
unaryFuncType Adr t = (Pointer t, t)
unaryFuncType Der t = (elemType t, t)

binaryFuncType :: BinaryOp -> Type -> Type -> (Type, (Type, Type))
binaryFuncType op a b = binaryOp op where
    binaryOp Com = (b, (a, b))
    binaryOp Eq = (bool, (commonTy, commonTy))
    binaryOp Ne = (bool, (commonTy, commonTy))
    binaryOp Gt = (bool, (commonTy, commonTy))
    binaryOp Ge = (bool, (commonTy, commonTy))
    binaryOp Lt = (bool, (commonTy, commonTy))
    binaryOp Le = (bool, (commonTy, commonTy))
    binaryOp _  = (commonTy, (commonTy, commonTy))
    commonTy = commonType a b

{- State operation -}

newScope :: (Monad m) => AnalysisT m ()
newScope = liftAnalysisState $ modify $ \s -> s {
        countStack = 0 : countStack s
    }
        -- a = analyseStatement (_ :: StatementF a -> ProgramAnalysis a)

{- Utils -}

cataM :: (Monad m, Traversable (Base t), Recursive t)
      => (Base t a -> m a) -> t -> m a
cataM alg = c where
  c = alg <=< traverse c . project

tzipWith :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tzipWith f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)
