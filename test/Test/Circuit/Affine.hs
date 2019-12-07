module Test.Circuit.Affine where

import           Circuit.Affine
import qualified Data.Map              as Map
import           Protolude
import           Test.Tasty.QuickCheck

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

arbAffineCircuit ::
  Arbitrary f =>
  Int ->
  Int ->
  Gen (AffineCircuit Int f)
arbAffineCircuit numVars size
  | size <= 0 =
    oneof $
      [ ConstGate <$> arbitrary
      ]
        ++ if numVars > 0
          then [Var <$> choose (0, numVars - 1)]
          else []
  | size > 0 =
    oneof
      [ ScalarMul <$> arbitrary <*> arbAffineCircuit numVars (size - 1),
        Add <$> arbAffineCircuit numVars (size - 1)
          <*> arbAffineCircuit numVars (size - 1)
      ]

arbInputVector :: Arbitrary f => Int -> Gen (Map Int f)
arbInputVector numVars = Map.fromList . zip [0 ..] <$> vector numVars

-- | The input vector has to have the correct length, so we want to
-- generate the program and the test input simultaneously.
data AffineCircuitWithInputs f = AffineCircuitWithInputs (AffineCircuit Int f) [Map Int f]
  deriving (Show)

instance Arbitrary f => Arbitrary (AffineCircuitWithInputs f) where
  arbitrary = do
    numVars <- abs <$> arbitrary
    program <- scale (`div` 7) $ sized (arbAffineCircuit numVars)
    inputs <- vectorOf 10 $ arbInputVector numVars
    pure $ AffineCircuitWithInputs program inputs

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-- | Check that evaluating the vector representation of the circuit
-- yields the same results as evaluating the circuit "directly". Field
-- is instantiated as being the rationals for testing. It later should
-- probably be something like Pairing.Fr.Fr.
prop_affineCircuitToAffineMap ::
  AffineCircuitWithInputs Rational -> Bool
prop_affineCircuitToAffineMap (AffineCircuitWithInputs program inputs) =
  all testInput inputs
  where
    testInput input =
      evalAffineCircuit Map.lookup input program
        == evalAffineMap (affineCircuitToAffineMap program) input
