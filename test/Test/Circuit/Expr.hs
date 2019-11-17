module Test.Circuit.Expr where

import Protolude
import Test.Circuit.Affine
import Test.Tasty.QuickCheck
import qualified Data.Map as Map
import Circuit.Expr hiding (fresh)
import Circuit.Arithmetic
import QAP
import Fresh
import Data.Curve.Weierstrass.BN254 (Fr)

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

arbExprBool :: Arbitrary f => Int -> Int -> Gen (Expr Int f Bool)
arbExprBool numVars size
  | size <= 0 = oneof $ [EConstBool <$> arbitrary] ++ if numVars > 0
    then []
    else []
  | size > 0 = oneof
    [ EBinOp BAnd <$> arbExprBool numVars (size - 1) <*> arbExprBool
      numVars
      (size - 1)
    , EBinOp BOr <$> arbExprBool numVars (size - 1) <*> arbExprBool numVars
                                                                    (size - 1)
    , EUnOp UNot <$> arbExprBool numVars (size - 1)
    , EEq <$> arbExpr numVars (size - 1)
          <*> arbExpr numVars (size - 1)
    ]

arbExpr :: Arbitrary f => Int -> Int -> Gen (Expr Int f f)
arbExpr numVars size
  | size <= 0 = oneof $ [EConst <$> arbitrary] ++ if numVars > 0
    then [EVar <$> choose (0, numVars - 1)]
    else []
  | size > 0 = oneof
    [ EBinOp BAdd <$> arbExpr numVars (size - 1) <*> arbExpr numVars (size - 1)
    , EBinOp BSub <$> arbExpr numVars (size - 1) <*> arbExpr numVars (size - 1)
    , EBinOp BMul <$> arbExpr numVars (size - 1) <*> arbExpr numVars (size - 1)
    , EUnOp UNeg <$> arbExpr numVars (size - 1)
    , EIf
    <$> arbExprBool numVars (size - 1)
    <*> arbExpr     numVars (size - 1)
    <*> arbExpr     numVars (size - 1)
    ]


data ExprWithInputs f = ExprWithInputs (Expr Int f f) [Map Int f]
  deriving Show


instance Arbitrary f => Arbitrary (ExprWithInputs f) where
  arbitrary = do
    numVars <- abs <$> arbitrary
    program <- scale (`div` 10) $ sized (arbExpr numVars)
    inputs <- vectorOf 5 $ arbInputVector numVars
    pure $ ExprWithInputs program inputs

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------


-- | Check whether exprToArithCircuit produces valid circuits
prop_compiledCircuitValid :: ExprWithInputs Fr -> Bool
prop_compiledCircuitValid (ExprWithInputs expr _) =
  validArithCircuit (execCircuitBuilder $ exprToArithCircuit expr (OutputWire 0))

-- | Check whether exprToArithCircuit produces circuits that have valid assignments to the qap
prop_compiledQAPValid :: ExprWithInputs Fr -> Property
prop_compiledQAPValid (ExprWithInputs expr inputs) = withMaxSuccess 50
  $ all testInput inputs
 where
  circuit = (execCircuitBuilder $ exprToArithCircuit expr (OutputWire 0))
  roots :: ArithCircuit Fr -> [[Fr]]
  roots = evalFresh . generateRoots (fromIntegral <$> fresh)
  qap = arithCircuitToQAP (roots circuit) circuit
  assignment input = generateAssignment circuit input
  testInput = verifyAssignment qap . assignment

-- | Check whether evaluating an expression and
-- evaluating the arithmetic circuit translation produces the same
-- result
prop_evalEqArithEval :: ExprWithInputs Fr -> Bool
prop_evalEqArithEval (ExprWithInputs expr inputs) = all testInput inputs
 where
  testInput input = exprResult input == arithResult input
  exprResult input = evalExpr (Map.lookup) expr input
  arithResult input = arithOutput input Map.! (OutputWire 0)
  arithOutput input = evalArithCircuit (Map.lookup)
                                       (Map.insert)
                                       circuit
                                       (Map.mapKeys InputWire input)
  circuit = (execCircuitBuilder $ exprToArithCircuit expr (OutputWire 0))
