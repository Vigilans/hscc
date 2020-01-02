module Language.C.Semantic.Evaluate where

import Language.C.Semantic.Analysis
import Language.C.Syntax
import Language.C.Syntax.Utils (bool, int)
import Data.Functor.Foldable
import Control.Monad.Identity
import Control.Monad.Zip

evalStatement :: Statement -> IdentityT ProgramAnalysis Statement
evalStatement = analyseStatement eval where
    eval (Compound stmts) = Compound <$> forM stmts evalStatement
    -- eval (ExprStmt expr)  = ExprStmt <$> forM expr evalExpression

-- | Evaluate the expression down.
-- The expression's AST will be modified to be suitable for codegen;
-- The final expression's type is returned.
evalExpression :: Expression -> IdentityT ProgramAnalysis (Typed Expression)
evalExpression = analyseExpression eval where
    eval _ (Literal _ (Variable id))   = return $ Fix $ Literal Unknown (Variable id)
    eval _ (Literal t lit)             = return $ Fix $ Literal t lit
    eval _ (Constant (_, e))           = return $ constEval e
    eval _ (Sizeof (Right (t, _)))     = return $ constEval (Fix . Sizeof . Left $ t)
    eval _ (Sizeof (Left t))           = return $ constEval (Fix . Sizeof . Left $ t)
    eval _ (Cast t' (t, e))            = return $ typeCast t' (t, e)
    eval _ (Unary  op a@(x,_))         = return $ Fix $ Unary  op $                  typeCast (snd $ unaryFuncType  op x)    a
    eval _ (Binary op a@(x,_) b@(y,_)) = return $ Fix $ Binary op `uncurry` tzipWith typeCast (snd $ binaryFuncType op x y) (a, b)
    eval _ (Assign op (t, l) r)        = return $ Fix $ Assign op l (typeCast t r)
    eval t (Cond c a b)                = return $ Fix $ Cond (typeCast bool c) (typeCast t a) (typeCast t b)
    eval _ (Call (t, fn) args)         = return $ Fix $ Call fn (zipWith typeCast (argTypes t) args)
    eval _ (Index (t, arr) (_, idx))   = return $ Fix $ Index arr idx

typeCast :: Type -> Typed Expression -> Expression
typeCast t' (t, e)
    | t' == t   = e
    | otherwise = Fix $ Cast t' e

-- | Evaluate the expression down in compile-time,
-- The final expression will be constants, or stop if any non-const reference is found
constEval :: Expression -> Expression
constEval = cata eval where
    eval (Constant e)                   = e
    eval (Sizeof (Left t))              = Fix $ Literal int $ IntegerLit $ sizeof t
    eval (Sizeof (Right (Fix (Literal t _)))) = Fix $ Literal int $ IntegerLit $ sizeof t
    -- eval (Unary Pos (Literal t lit))    = _
    eval (Unary Neg e)                  = _
    eval (Unary Not e)                  = _
    eval (Unary Inv e)                  = _
    eval (Unary _ e)                    = _
    eval (Cast t' e)                    = _
    eval (Assign op l r)                = _
    eval (Binary op a b)                = _
    eval (Cond c a b)                   = _
    eval (Index arr idx)                = _
    eval (Literal _ (Variable id))      = _
    eval (Literal t lit)                = _
    eval e = Fix  e -- Fallback case, cannot evaluate in compile time
--  eval (StrLiteral str) = (string str, StrLiteral str)
-- constEval (Constant t lit) = (t, Constant t lit)

instance (MonadAnalysis m) => MonadAnalysis (IdentityT m)
