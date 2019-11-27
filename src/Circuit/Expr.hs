{-# LANGUAGE StandaloneDeriving #-}

module Circuit.Expr
  ( UnOp (..),
    BinOp (..),
    Expr (..),
    ExprM,
    compile,
    emit,
    imm,
    addVar,
    addWire,
    freshInput,
    freshOutput,
    rotateList,
    runCircuitBuilder,
    evalCircuitBuilder,
    execCircuitBuilder,
    exprToArithCircuit,
    evalExpr,
    truncRotate,
  )
where

import Circuit.Affine
import Circuit.Arithmetic
import Protolude
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

data UnOp f a where
  UNeg :: UnOp f f
  UNot :: UnOp f Bool
  -- | # truncate bits, # rotate bits
  URot :: Int -> Int -> UnOp f f

data BinOp f a where
  BAdd :: BinOp f f
  BSub :: BinOp f f
  BMul :: BinOp f f
  BAnd :: BinOp f Bool
  BOr :: BinOp f Bool
  BXor :: BinOp f Bool

opPrecedence :: BinOp f a -> Int
opPrecedence BOr = 5
opPrecedence BXor = 5
opPrecedence BAnd = 5
opPrecedence BSub = 6
opPrecedence BAdd = 6
opPrecedence BMul = 7

-- | Expression data type of (arithmetic) expressions over a field @f@
-- with variable names/indices coming from @i@.
data Expr i f ty where
  EConst :: f -> Expr i f f
  EConstBool :: Bool -> Expr i f Bool
  EVar :: i -> Expr i f f
  EVarBool :: i -> Expr i f Bool
  EUnOp :: UnOp f ty -> Expr i f ty -> Expr i f ty
  EBinOp :: BinOp f ty -> Expr i f ty -> Expr i f ty -> Expr i f ty
  EIf :: Expr i f Bool -> Expr i f ty -> Expr i f ty -> Expr i f ty
  EEq :: Expr i f f -> Expr i f f -> Expr i f Bool

deriving instance (Show i, Show f) => Show (Expr i f ty)

deriving instance (Show f) => Show (BinOp f a)

deriving instance (Show f) => Show (UnOp f a)

instance Pretty (BinOp f a) where
  pretty op = case op of
    BAdd -> text "+"
    BSub -> text "-"
    BMul -> text "*"
    BAnd -> text "&&"
    BOr -> text "||"
    BXor -> text "xor"

instance Pretty (UnOp f a) where
  pretty op = case op of
    UNeg -> text "neg"
    UNot -> text "!"
    URot truncBits rotBits -> text "rot(" <> pretty truncBits <> text "," <> pretty rotBits <> ")"

instance (Pretty f, Pretty i, Pretty ty) => Pretty (Expr i f ty) where
  pretty = prettyPrec 0
    where
      prettyPrec :: (Pretty f, Pretty i, Pretty ty) => Int -> Expr i f ty -> Doc
      prettyPrec p e =
        case e of
          EVar v ->
            pretty v
          EVarBool v ->
            pretty v
          EConst l ->
            pretty l
          EConstBool b ->
            pretty b
          -- TODO correct precedence
          EUnOp op e1 -> parens (pretty op <+> pretty e1)
          EBinOp op e1 e2 ->
            parensPrec (opPrecedence op) p $
              prettyPrec (opPrecedence op) e1
                <+> pretty op
                <+> prettyPrec (opPrecedence op) e2
          EIf b true false ->
            parensPrec 4 p (text "if" <+> pretty b <+> text "then" <+> pretty true <+> text "else" <+> pretty false)
          -- TODO correct precedence
          EEq l r ->
            parensPrec 1 p (pretty l) <+> text "=" <+> parensPrec 1 p (pretty r)

parensPrec :: Int -> Int -> Doc -> Doc
parensPrec opPrec p = if p > opPrec then parens else identity

-------------------------------------------------------------------------------
-- Evaluator
-------------------------------------------------------------------------------

-- | Truncate a number to the given number of bits and perform a right
-- rotation (assuming small-endianness) within the truncation.
truncRotate ::
  (Bits f) =>
  -- | number of bits to truncate to
  Int ->
  -- | number of bits to rotate by
  Int ->
  f ->
  f
truncRotate nbits nrots x =
  foldr
    ( \ix rest ->
        if testBit x ix
          then setBit rest ((ix + nrots) `mod` nbits)
          else rest
    )
    zeroBits
    [0 .. nbits - 1]

-- | Evaluate arithmetic expressions directly, given an environment
evalExpr ::
  (Bits f, Num f) =>
  -- | variable lookup
  (i -> vars -> Maybe f) ->
  -- | expression to evaluate
  Expr i f ty ->
  -- | input values
  vars ->
  -- | resulting value
  ty
evalExpr lookupVar expr vars = case expr of
  EConst f -> f
  EConstBool b -> b
  EVar i -> case lookupVar i vars of
    Just v -> v
    Nothing -> panic "TODO: incorrect var lookup"
  EVarBool i -> case lookupVar i vars of
    Just v -> v == 1
    Nothing -> panic "TODO: incorrect var lookup"
  EUnOp UNeg e1 ->
    negate $ evalExpr lookupVar e1 vars
  EUnOp UNot e1 ->
    not $ evalExpr lookupVar e1 vars
  EUnOp (URot truncBits rotBits) e1 ->
    truncRotate truncBits rotBits $ evalExpr lookupVar e1 vars
  EBinOp op e1 e2 ->
    (evalExpr lookupVar e1 vars) `apply` (evalExpr lookupVar e2 vars)
    where
      apply = case op of
        BAdd -> (+)
        BSub -> (-)
        BMul -> (*)
        BAnd -> (&&)
        BOr -> (||)
        BXor -> \x y -> (x || y) && not (x && y)
  EIf b true false ->
    if evalExpr lookupVar b vars
      then evalExpr lookupVar true vars
      else evalExpr lookupVar false vars
  EEq lhs rhs -> evalExpr lookupVar lhs vars == evalExpr lookupVar rhs vars

-------------------------------------------------------------------------------
-- Circuit Builder
-------------------------------------------------------------------------------

type ExprM f a = State (ArithCircuit f, Int) a

execCircuitBuilder :: ExprM f a -> ArithCircuit f
execCircuitBuilder m = reverseCircuit $ fst $ execState m (ArithCircuit [], 0)
  where
    reverseCircuit = \(ArithCircuit cs) -> ArithCircuit $ reverse cs

evalCircuitBuilder :: ExprM f a -> a
evalCircuitBuilder = fst . runCircuitBuilder

runCircuitBuilder :: ExprM f a -> (a, ArithCircuit f)
runCircuitBuilder m = second (reverseCircuit . fst) $ runState m (ArithCircuit [], 0)
  where
    reverseCircuit = \(ArithCircuit cs) -> ArithCircuit $ reverse cs

fresh :: ExprM f Int
fresh = do
  v <- gets snd
  modify (second (+ 1))
  pure v

-- | Fresh intermediate variables
imm :: ExprM f Wire
imm = IntermediateWire <$> fresh

-- | Fresh input variables
freshInput :: ExprM f Wire
freshInput = InputWire <$> fresh

-- | Fresh output variables
freshOutput :: ExprM f Wire
freshOutput = OutputWire <$> fresh

-- | Multiply two wires or affine circuits to an intermediate variable
mulToImm :: Either Wire (AffineCircuit Wire f) -> Either Wire (AffineCircuit Wire f) -> ExprM f Wire
mulToImm l r = do
  o <- imm
  emit $ Mul (addVar l) (addVar r) o
  pure o

-- | Add a Mul and its output to the ArithCircuit
emit :: Gate Wire f -> ExprM f ()
emit c = modify $ first (\(ArithCircuit cs) -> ArithCircuit (c : cs))

-- | Rotate a list to the right
rotateList :: Int -> [a] -> [a]
rotateList steps x = take (length x) $ drop steps $ cycle x

-- | Turn a wire into an affine circuit, or leave it be
addVar :: Either Wire (AffineCircuit Wire f) -> AffineCircuit Wire f
addVar (Left w) = Var w
addVar (Right c) = c

-- | Turn an affine circuit into a wire, or leave it be
addWire :: Num f => Either Wire (AffineCircuit Wire f) -> ExprM f Wire
addWire (Left w) = pure w
addWire (Right c) = do
  mulOut <- imm
  emit $ Mul (ConstGate 1) c mulOut
  pure mulOut

compile :: Num f => Expr Wire f ty -> ExprM f (Either Wire (AffineCircuit Wire f))
compile expr = case expr of
  EConst n -> pure . Right $ ConstGate n
  EConstBool b -> pure . Right $ ConstGate (if b then 1 else 0)
  EVar v -> pure . Left $ v
  EVarBool v -> pure . Left $ v
  EUnOp op e1 -> do
    e1Out <- compile e1
    case op of
      UNeg -> pure . Right $ ScalarMul (-1) (addVar e1Out)
      UNot -> pure . Right $ Add (ConstGate 1) (ScalarMul (-1) (addVar e1Out))
      URot truncBits rotBits -> do
        inp <- addWire e1Out
        outputs <- replicateM truncBits imm
        emit $ Split inp outputs
        pure . Right $ unsplit (rotateList rotBits outputs)
  EBinOp op e1 e2 -> do
    e1Out <- addVar <$> compile e1
    e2Out <- addVar <$> compile e2
    case op of
      BAdd -> pure . Right $ Add e1Out e2Out
      BMul -> do
        tmp1 <- mulToImm (Right e1Out) (Right e2Out)
        pure . Left $ tmp1
      -- SUB(x, y) = x + (-y)
      BSub -> pure . Right $ Add e1Out (ScalarMul (-1) e2Out)
      BAnd -> do
        tmp1 <- mulToImm (Right e1Out) (Right e2Out)
        pure . Left $ tmp1
      BOr -> do
        -- OR(input1, input2) = (input1 + input2) - (input1 * input2)
        tmp1 <- imm
        emit $ Mul e1Out e2Out tmp1
        pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-1) (Var tmp1))
      BXor -> do
        -- XOR(input1, input2) = (input1 + input2) - 2 * (input1 * input2)
        tmp1 <- imm
        emit $ Mul e1Out e2Out tmp1
        pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-2) (Var tmp1))
  -- IF(cond, true, false) = (cond*true) + ((!cond) * false)
  EIf cond true false -> do
    condOut <- addVar <$> compile cond
    trueOut <- addVar <$> compile true
    falseOut <- addVar <$> compile false
    tmp1 <- imm
    tmp2 <- imm
    emit $ Mul condOut trueOut tmp1
    emit $ Mul (Add (ConstGate 1) (ScalarMul (-1) condOut)) falseOut tmp2
    pure . Right $ Add (Var tmp1) (Var tmp2)
  -- EQ(lhs, rhs) = (lhs - rhs == 1)
  EEq lhs rhs -> do
    lhsSubRhs <- compile (EBinOp BSub lhs rhs)
    eqInWire <- addWire lhsSubRhs
    eqFreeWire <- imm
    eqOutWire <- imm
    emit $ Equal eqInWire eqFreeWire eqOutWire
    -- eqOutWire == 0 if lhs == rhs, so we need to return 1 -
    -- neqOutWire instead.
    pure . Right $ Add (ConstGate 1) (ScalarMul (-1) (Var eqOutWire))

-- | Translate an arithmetic expression to an arithmetic circuit
exprToArithCircuit ::
  Num f =>
  -- | expression to compile
  Expr Int f ty ->
  -- | Wire to assign the output of the expression to
  Wire ->
  ExprM f ()
exprToArithCircuit expr output =
  exprToArithCircuit' (mapVarsExpr InputWire expr) output

exprToArithCircuit' :: Num f => Expr Wire f ty -> Wire -> ExprM f ()
exprToArithCircuit' expr output = do
  exprOut <- compile expr
  emit $ Mul (ConstGate 1) (addVar exprOut) output

-- | Apply function to variable names.
mapVarsExpr :: (i -> j) -> Expr i f ty -> Expr j f ty
mapVarsExpr f expr = case expr of
  EVar i -> EVar $ f i
  EVarBool i -> EVarBool $ f i
  EConst v -> EConst v
  EConstBool b -> EConstBool b
  EBinOp op e1 e2 -> EBinOp op (mapVarsExpr f e1) (mapVarsExpr f e2)
  EUnOp op e1 -> EUnOp op (mapVarsExpr f e1)
  EIf b tr fl -> EIf (mapVarsExpr f b) (mapVarsExpr f tr) (mapVarsExpr f fl)
  EEq lhs rhs -> EEq (mapVarsExpr f lhs) (mapVarsExpr f rhs)
