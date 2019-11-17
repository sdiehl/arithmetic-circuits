-- | Surface language
module Circuit.Lang
  ( c
  , add
  , sub
  , mul
  , and_
  , or_
  , xor_
  , not_
  , eq
  , deref
  , e
  , cond
  , ret
  , input
  ) where

import Protolude

import Circuit.Affine (AffineCircuit(..))
import Circuit.Arithmetic (Wire(..), Gate(..))
import Circuit.Expr

c :: f -> Expr Wire f f
c = EConst

add, sub, mul :: Expr Wire f f -> Expr Wire f f -> Expr Wire f f
add = EBinOp BAdd
sub = EBinOp BSub
mul = EBinOp BMul

-- Have to use underscore or similar to avoid shadowing @and@ and @or@
-- from Prelude/Protolude.
and_, or_, xor_ :: Expr Wire f Bool -> Expr Wire f Bool -> Expr Wire f Bool
and_ = EBinOp BAnd
or_ = EBinOp BOr
xor_ = EBinOp BXor

not_ :: Expr Wire f Bool -> Expr Wire f Bool
not_ = EUnOp UNot

eq :: Expr Wire f f -> Expr Wire f f -> Expr Wire f Bool
eq = EEq

deref :: Wire -> Expr Wire f f
deref = EVar

e :: Num f => Expr Wire f f -> ExprM f Wire
e = compileWithWire imm

cond :: Expr Wire f Bool -> Expr Wire f ty -> Expr Wire f ty -> Expr Wire f ty
cond = EIf

ret :: Num f => Expr Wire f f -> ExprM f Wire
ret = compileWithWire freshOutput

compileWithWire :: Num f => ExprM f Wire -> Expr Wire f f -> ExprM f Wire
compileWithWire freshWire expr = do
  compileOut <- compile expr
  case compileOut of
    Left wire -> pure wire
    Right circ -> do
           wire <- freshWire
           emit $ Mul (ConstGate 1) circ wire
           pure wire

input :: ExprM f Wire
input = freshInput
