{-# LANGUAGE PackageImports #-}
module Test.QAP where

import Protolude

import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import Circuit.Affine
import Circuit.Arithmetic
import Data.Pairing.BN254 (Fr, getRootOfUnity)
import QAP

import Test.Circuit.Affine (arbAffineCircuit, arbInputVector)

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

arbGate
  :: Arbitrary f
  => Int -> Gen (Gate Wire f)
arbGate numVars
  = oneof
    [ Mul <$> (mapVarsAffine InputWire <$> sized (arbAffineCircuit numVars))
          <*> (mapVarsAffine InputWire <$> sized (arbAffineCircuit numVars))
          <*> pure (OutputWire 0)
    , Equal <$> (InputWire <$> choose (0, numVars - 1))
            <*> pure (IntermediateWire 0)
            <*> pure (OutputWire 0)
    ]

data GateWithInputs f = GateWithInputs (Gate Wire f) [Map Int f]
  deriving Show

instance Arbitrary f => Arbitrary (GateWithInputs f) where
  arbitrary = do
    numVars <- (+1) . abs <$> arbitrary
    program <- scale (`div` 7) $ arbGate numVars
    inputs <- vectorOf 10 (arbInputVector numVars)
    pure $ GateWithInputs program inputs

-------------------------------------------------------------------------------
-- Test values
-------------------------------------------------------------------------------

testArithCircuit :: ArithCircuit Fr
testArithCircuit
  = ArithCircuit
    [ Mul (Var (InputWire 0)) (Var (InputWire 1)) (IntermediateWire 0)
    , Mul (Var (InputWire 2)) (Var (InputWire 3)) (IntermediateWire 1)
    , Mul (Add (ConstGate 10) (Var (IntermediateWire 0))) (Var (IntermediateWire 1)) (OutputWire 0)
    ]

testVarsArithCircuit :: Map Int Fr
testVarsArithCircuit
  = Map.fromList [ (0, 2)
                 , (1, 3)
                 , (2, 4)
                 , (3, 5)
                 ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

unit_arithCircuitToQapCorrect :: Assertion
unit_arithCircuitToQapCorrect
  = assertBool "Verifying assignment against QAP of circuit failed"
  $ verifyAssignment qap assignment
  where
    roots = [[7],[8],[9]]
    qap = arithCircuitToQAP roots testArithCircuit
    assignment = generateAssignment testArithCircuit testVarsArithCircuit

unit_arithCircuitToQapNoFalsePositive :: Assertion
unit_arithCircuitToQapNoFalsePositive
    = assertBool "Verifying whether verification fails on faulty assignment"
    $ not $ verifyAssignment qap invalidAssignment
  where
    roots = [[7],[8],[9]]
    qap = arithCircuitToQAP roots testArithCircuit
    invalidAssignment
      = QapSet
        { qapSetConstant = 1
        , qapSetInput = Map.fromList [(0,2),(1,3),(2,4),(3,5)]
        , qapSetIntermediate = Map.fromList [(0,7),(1,20)]
        , qapSetOutput = Map.fromList [(0,320)]
        }

prop_gateToQapCorrect
  :: GateWithInputs Fr -> Bool
prop_gateToQapCorrect (GateWithInputs program inputs)
  = all testInput inputs
  where
    roots = case program of
             Mul {} -> [1]
             Equal {} -> [1,2]
             _ -> panic "Invalid roots"
    qap = gateToQAP getRootOfUnity roots program
    assignment input = generateAssignmentGate program input
    testInput = verifyAssignment qap . assignment
