{-# LANGUAGE RecursiveDo #-}

module Language.C.CodeGen.LLVM where

import Language.C.Program
import Language.C.Syntax
import Language.C.Semantic
import Data.Char
import Data.Maybe
import Data.Bifunctor
import Data.String.Transform

import Control.Monad.Identity
import Control.Monad.State

import qualified LLVM.AST                        as LLVM
import qualified LLVM.AST.Constant               as LLVM
import qualified LLVM.AST.Float                  as LLVM
import qualified LLVM.AST.Type                   as LLVM
import qualified LLVM.AST.Typed                  as LLVM
import qualified LLVM.AST.IntegerPredicate       as LLVM
import qualified LLVM.AST.FloatingPointPredicate as LLVM hiding (UGT, UGE, ULT, ULE)
import LLVM.IRBuilder.Instruction                as LLVMIR
import LLVM.IRBuilder.Module                     as LLVMIR
import LLVM.IRBuilder.Monad                      as LLVMIR
import LLVM.IRBuilder.Constant                   as LLVMIR

type ModGen = ModuleBuilderT ProgramAnalysis

type CodeGen = IRBuilderT ModGen

codegen :: String -> CompilationUnit -> LLVM.Module
codegen (toShortByteString -> filename) unit = buildModule filename _

genExpression :: Expression -> CodeGen (Typed LLVM.Operand) -- Expression will return a typed value
genExpression = analyseExpression gen where
    gen _ (Literal t lit)             = genLiteral t lit
    gen _ (Cast t' (t, e))            = genTypeCast t t' e
    gen _ (Unary  op (t, e))          = genUnaryOp  op t e
    gen _ (Binary op (t, a) (_, b))   = genBinaryOp op t a b
    gen _ (Assign op (t, l) (_, r))   = genAssignOp op t l r
    gen _ (Cond (_, c) (_, a) (_, b)) = genTenaryOp c b a
    gen _ (Call (_, fn) args)         = genInvokeOp fn (snd <$> args)
    gen _ (Index (_, arr) (_, idx))   = extractElement arr idx

genLiteral :: Type -> Lit -> CodeGen LLVM.Operand
genLiteral t (IntegerLit lit) = LLVM.ConstantOperand <$> return (LLVM.Int (fromInteger $ sizeof t) lit)
genLiteral t (CharLit lit)    = LLVM.ConstantOperand <$> return (LLVM.Int (fromInteger $ sizeof t) (toInteger $ ord lit))
genLiteral t (FloatLit lit)   = LLVM.ConstantOperand <$> return (LLVM.Float (genFloating t lit))
genLiteral _ (StringLit lit)  = LLVM.ConstantOperand <$> (globalStringPtr lit =<< freshUnName)

genUnaryOp :: UnaryOp -> Type -> LLVM.Operand -> CodeGen LLVM.Operand
genUnaryOp op t a = do { a' <- genOperand a; gen op t a' } where
    gen Pos _ = return -- return the same operand
    gen Neg t@(Integer _ _)    = \v -> genLiteral t (IntegerLit 0) >>= flip sub v
    gen Neg t@(Floating _)     = \v -> genLiteral t (FloatLit 0) >>= flip fsub v
    gen Inv t@(Integer _ _)    = \v -> xor v =<< genLiteral t (IntegerLit (-1))
    gen Not t@(Integer Bool _) = \v -> xor v =<< genLiteral t (IntegerLit 1)
    gen Adr _ = \(LLVM.LocalReference t n) -> return (LLVM.LocalReference (LLVM.ptr t) n)
    gen Der t@(Pointer _) = \v -> load v (fromInteger $ sizeof t)

genBinaryOp :: BinaryOp -> Type -> LLVM.Operand -> LLVM.Operand -> CodeGen LLVM.Operand
genBinaryOp op t a b = do { a' <- genOperand a; b' <- genOperand b; gen op t a' b' } where
    gen Nop _ = \a b -> return a -- Acts like const, return the left operand
    gen Com _ = \a b -> return b
    gen Add (Integer _ _) = add
    gen Add (Floating _) = fadd
    gen Sub (Integer _ _) = sub
    gen Sub (Floating _) = fsub
    gen Mul (Integer _ _) = mul
    gen Mul (Floating _) = fmul
    gen Div (Integer _ Unsigned) = udiv
    gen Div (Integer _ Signed)   = sdiv
    gen Div (Floating _)         = fdiv
    gen Mod (Integer _ Unsigned) = urem
    gen Mod (Integer _ Signed)   = srem
    gen Mod (Floating _)         = frem
    gen Bnd (Integer _ _) = LLVMIR.and
    gen Bor (Integer _ _) = LLVMIR.or
    gen Xor (Integer _ _) = LLVMIR.xor
    gen And (Integer Bool _) = LLVMIR.and
    gen Or  (Integer Bool _) = LLVMIR.or
    gen Eq (Integer _ _) = icmp LLVM.EQ
    gen Eq (Floating _)  = fcmp LLVM.OEQ
    gen Ne (Integer _ _) = icmp LLVM.NE
    gen Ne (Floating _)  = fcmp LLVM.ONE
    gen Gt (Integer _ Unsigned) = icmp LLVM.UGT
    gen Gt (Integer _ Signed)   = icmp LLVM.SGT
    gen Gt (Floating _)         = fcmp LLVM.OGT
    gen Ge (Integer _ Unsigned) = icmp LLVM.UGE
    gen Ge (Integer _ Signed)   = icmp LLVM.SGE
    gen Ge (Floating _)         = fcmp LLVM.OGE
    gen Lt (Integer _ Unsigned) = icmp LLVM.ULT
    gen Lt (Integer _ Signed)   = icmp LLVM.SLT
    gen Lt (Floating _)         = fcmp LLVM.OLT
    gen Le (Integer _ Unsigned) = icmp LLVM.ULE
    gen Le (Integer _ Signed)   = icmp LLVM.SLE
    gen Le (Floating _)         = fcmp LLVM.OLE

genAssignOp :: BinaryOp -> Type -> LLVM.Operand -> LLVM.Operand -> CodeGen LLVM.Operand
genAssignOp op t a b = let align = fromInteger (8 * sizeof t) in do
    a' <- case op of
        Nop -> genOperand b
        op  -> genBinaryOp op t a b
    store a align a'
    return a

genTenaryOp :: LLVM.Operand -> LLVM.Operand -> LLVM.Operand -> CodeGen LLVM.Operand
genTenaryOp a b c = do { a' <- genOperand a; b' <- genOperand b; c' <- genOperand c; select a b c }

genInvokeOp :: LLVM.Operand -> [LLVM.Operand] -> CodeGen LLVM.Operand
genInvokeOp fn args = call fn =<< mapM ((return . (, [])) <=< genOperand) args

genOperand :: LLVM.Operand -> CodeGen LLVM.Operand
genOperand a@(LLVM.LocalReference _ _) = load a 4 -- Load variable before calculating op
genOperand a = return a

genBoolCast :: Type -> LLVM.Operand -> CodeGen LLVM.Operand
genBoolCast t@(Integer _ _) opr = genLiteral t (IntegerLit 0) >>= icmp LLVM.NE opr
genBoolCast t@(Floating _)  opr = genLiteral t (FloatLit 0) >>= fcmp LLVM.ONE opr

genTypeCast :: Type -> Type -> LLVM.Operand -> CodeGen LLVM.Operand
genTypeCast from to opr = cast from to opr (genType to) where
    cast t (Integer Bool _)                = flip . const $ genBoolCast t
    cast (Integer a _) (Integer b s)       | a > b = trunc | s == Signed = sext | otherwise = zext
    cast (Floating a)  (Floating b)        | a > b = fptrunc | otherwise = fpext
    cast (Floating a) (Integer b Unsigned) = fptoui
    cast (Floating a) (Integer b Signed)   = fptosi
    cast (Integer a Unsigned) (Floating b) = uitofp
    cast (Integer a Signed)   (Floating b) = sitofp
    cast (Integer a _) (Pointer t)         = inttoptr
    cast (Pointer t)   (Integer a _)       = ptrtoint

genType :: Type -> LLVM.Type
genType Void   = LLVM.void
genType (Integer Bool  _) = LLVM.i1
genType (Integer Char  _) = LLVM.i8
genType (Integer Short _) = LLVM.i16
genType (Integer Int   _) = LLVM.i32
genType (Integer Long  _) = LLVM.i32
genType (Floating Float)  = LLVM.float
genType (Floating Double) = LLVM.double
genType (Pointer t) = LLVM.ptr $ genType t
genType (Array t n) = LLVM.ArrayType (fromInteger n) (genType t)

genFloating :: Type -> Double -> LLVM.SomeFloat
genFloating (Floating Float)  = LLVM.Single . realToFrac
genFloating (Floating Double) = LLVM.Double

genParamName :: Maybe Identifier -> ParameterName
genParamName Nothing  = NoParameterName
genParamName (Just n) = ParameterName $ toShortByteString n

instance (MonadAnalysis m) => MonadAnalysis (ModuleBuilderT m)

instance (MonadAnalysis m) => MonadAnalysis (IRBuilderT m)
