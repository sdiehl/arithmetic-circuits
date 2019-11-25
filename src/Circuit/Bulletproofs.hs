{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, RecordWildCards,
             ScopedTypeVariables, TypeApplications, ViewPatterns #-}

-- | Translate arithmetic circuits into a Hadamard product equation
-- and linear constraints.
module Circuit.Bulletproofs
  ( setupProof,
    SetupProof (..),
    AltArithCircuit,
    LinearConstraint (..),
    GateConstraint (..),
    rewire,
    rewireCircuit,
    circuitToConstraints,
    transformInputs,
    evalCircuit,
    computeBulletproofsAssignment,
  )
where

import qualified Bulletproofs.ArithmeticCircuit   as Bulletproofs
import           Bulletproofs.Utils               (commit)
import           Circuit.Affine                   (AffineCircuit(..),
                                                   affineCircuitToAffineMap,
                                                   dotProduct,
                                                   evalAffineCircuit)
import           Circuit.Arithmetic               (ArithCircuit(..), Gate(..),
                                                   Wire(..), collectInputsGate,
                                                   mapVarsGate, outputWires)
import           Control.Monad.Random             (MonadRandom, getRandomR)
import           Data.Curve.Weierstrass.SECP256K1 (Fr, PA)
import qualified Data.Map                         as Map
import           Protolude
import           Text.PrettyPrint.Leijen.Text     as PP (Pretty(..), enclose,
                                                         lbracket, rbracket,
                                                         text, vcat, (<+>))

newtype AltArithCircuit f = AltArithCircuit [Gate AltWire f]
  deriving (Show, Generic, NFData)

instance (Pretty f, Show f) => Pretty (AltArithCircuit f) where
  pretty (AltArithCircuit l) = pretty l

-- | Use different wire type as required for the constraints generated
-- in this module.
rewireCircuit :: ArithCircuit f -> AltArithCircuit f
rewireCircuit (ArithCircuit oldGates) = AltArithCircuit newGates
  where
    newGates = map (mapVarsGate (rewire maxMid)) oldGates
    getMid (IntermediateWire x) = x
    getMid _ = 0
    maxMid :: Int
    maxMid = maximumSafe . map getMid . concatMap outputWires $ oldGates

-- | Replace all input wires v_i with a mul-gate (v_i * 1). This means
-- that when we translate it to linear constraints, the weights matrix
-- for V will always be of rank m, where m is the number of input
-- wires, as is required by the Bulletproof protocol.
transformInputs :: forall f. Num f => AltArithCircuit f -> AltArithCircuit f
transformInputs (AltArithCircuit oldGates) = AltArithCircuit newGates
  where
    newGates :: [Gate AltWire f]
    newGates = inputGates ++ map rewireInput oldGates
    maxInp :: Int
    maxInp = maximumSafe . mapMaybe getInp . concatMap collectInputsGate $ oldGates
    getInp (InWire x) = Just x
    getInp _ = Nothing
    maxOutp :: Int
    maxOutp = maximumSafe . mapMaybe getOutp . concatMap outputWires $ oldGates
    getOutp (OutWire x) = Just x
    getOutp _ = Nothing
    inputGates :: [Gate AltWire f]
    inputGates = map inputGate [0 .. maxInp]
    inputGate :: Int -> Gate AltWire f
    inputGate i = Mul (Var (InWire i)) (ConstGate 1) (OutWire (maxOutp + 1 + i))
    rewireInput :: Gate AltWire f -> Gate AltWire f
    rewireInput =
      mapVarsGate
        ( \case
            InWire i -> OutWire (maxOutp + 1 + i)
            w -> w
        )

maximumSafe :: (Num f, Ord f) => [f] -> f
maximumSafe [] = 0
maximumSafe ls = maximum ls

rewire :: Int -> Wire -> AltWire
rewire _maxMid (InputWire i) = InWire i
rewire maxMid (OutputWire i) = OutWire (i + maxMid + 1)
rewire _maxMid (IntermediateWire i) = OutWire i

-- | Distinguish between left/right/out and in wires.
data AltWire
  = LeftWire Int
  | RightWire Int
  | OutWire Int
  | InWire Int
  deriving (Show, Eq, Ord, Generic, NFData)

instance Pretty AltWire where
  pretty (LeftWire v) = text "left_" <> pretty v
  pretty (RightWire v) = text "right_" <> pretty v
  pretty (OutWire v) = text "out_" <> pretty v
  pretty (InWire v) = text "in_" <> pretty v

getAltWireNumber :: AltWire -> Int
getAltWireNumber = \case
  LeftWire i -> i
  RightWire i -> i
  OutWire i -> i
  InWire i -> i

-- Should we unify this type with the assignments, a la QapSet?
data LinearConstraint f
  = LinearConstraint
      { -- | wL
        lcWeightsLeft :: Map Int f,
        -- | wR
        lcWeightsRight :: Map Int f,
        -- | wO
        lcWeightsOut :: Map Int f,
        -- | wV
        lcWeightsIn :: Map Int f,
        -- | c
        lcConstant :: f
      }
  deriving (Show)

instance Pretty f => Pretty (LinearConstraint f) where
  pretty (LinearConstraint left right out lIn cnst) =
    vcat
      [ text "lc left:" <+> pretty (ppMap left),
        text "lc right:" <+> pretty (ppMap right),
        text "lc out:" <+> pretty (ppMap out),
        text "lc in:" <+> pretty (ppMap lIn),
        text "lc constant:" <+> pretty cnst
      ]
    where
      ppMap =
        vcat
          . map (\(ix, x) -> enclose lbracket rbracket (pretty ix) <+> pretty x)
          . Map.toList

data MulConstraint i
  = MulConstraint
      { -- | pointer to aLi
        mcLeft :: i,
        -- | pointer to aRi
        mcRight :: i,
        -- | pointer to aOi
        mcOut :: i
      }
  deriving (Show)

instance Pretty i => Pretty (MulConstraint i) where
  pretty (MulConstraint left right out) =
    vcat
      [ text "mc left:" <+> pretty left,
        text "mc right:" <+> pretty right,
        text "mc out:" <+> pretty out
      ]

data GateConstraint i f
  = GateConstraint
      { gcLinearConstraintLeft :: LinearConstraint f,
        gcLinearConstraintRight :: LinearConstraint f,
        gcMulConstraint :: MulConstraint i
      }
  deriving (Show)

instance (Pretty i, Pretty f) => Pretty (GateConstraint i f) where
  pretty (GateConstraint left right mul) =
    vcat
      [ text "linear constraint left:" <+> pretty left,
        text "linear constraint right:" <+> pretty right,
        text "mul constraint:" <+> pretty mul
      ]

-- | Map AltWire f is isomorphic to Assignment f, assuming the lengths
-- are correct. We can think of @Map Int f@ as a (potentially) sparse
-- vector.
data Assignment f
  = Assignment
      { -- | length is number of gates
        assignmentLeft :: Map Int f,
        -- | length is number of gates
        assignmentRight :: Map Int f,
        -- | length is number of gates
        assignmentOut :: Map Int f,
        -- | length is number of inputs
        assignmentIn :: Map Int f
      }
  deriving (Show)

assignmentToMap :: Assignment f -> Map AltWire f
assignmentToMap Assignment {..} =
  Map.unions
    [ Map.mapKeys LeftWire assignmentLeft,
      Map.mapKeys RightWire assignmentRight,
      Map.mapKeys OutWire assignmentOut,
      Map.mapKeys InWire assignmentIn
    ]

mapToAssignment :: Map AltWire f -> Assignment f
mapToAssignment wireMap = Assignment
  { assignmentLeft =
      Map.mapKeys getAltWireNumber . Map.filterWithKey isLeftWire $ wireMap,
    assignmentRight =
      Map.mapKeys getAltWireNumber . Map.filterWithKey isRightWire $ wireMap,
    assignmentOut =
      Map.mapKeys getAltWireNumber . Map.filterWithKey isOutWire $ wireMap,
    assignmentIn =
      Map.mapKeys getAltWireNumber . Map.filterWithKey isInWire $ wireMap
  }

linearConstraintToAffineMap :: LinearConstraint f -> (f, Map AltWire f)
linearConstraintToAffineMap LinearConstraint {..} =
  ( lcConstant,
    Map.unions
      [ Map.mapKeys LeftWire lcWeightsLeft,
        Map.mapKeys RightWire lcWeightsRight,
        Map.mapKeys OutWire lcWeightsOut,
        Map.mapKeys InWire lcWeightsIn
      ]
  )

affineMapToLinearConstraint :: Num f => (f, Map AltWire f) -> LinearConstraint f
affineMapToLinearConstraint (constant, wireMap) = LinearConstraint
  { lcWeightsLeft =
      fmap negate . Map.mapKeys getAltWireNumber . Map.filterWithKey isLeftWire $ wireMap,
    lcWeightsRight =
      fmap negate . Map.mapKeys getAltWireNumber . Map.filterWithKey isRightWire $ wireMap,
    lcWeightsOut =
      fmap negate . Map.mapKeys getAltWireNumber . Map.filterWithKey isOutWire $ wireMap,
    lcWeightsIn =
      Map.mapKeys getAltWireNumber . Map.filterWithKey isInWire $ wireMap,
    lcConstant =
      constant
  }

updateConstraint :: f -> LinearConstraint f -> AltWire -> LinearConstraint f
updateConstraint x lc = \case
  LeftWire i -> lc {lcWeightsLeft = Map.insert i x $ lcWeightsLeft lc}
  RightWire i -> lc {lcWeightsRight = Map.insert i x $ lcWeightsRight lc}
  OutWire i -> lc {lcWeightsOut = Map.insert i x $ lcWeightsOut lc}
  InWire i -> lc {lcWeightsIn = Map.insert i x $ lcWeightsIn lc}

isLeftWire :: AltWire -> f -> Bool
isLeftWire (LeftWire _) _ = True
isLeftWire _ _ = False

isRightWire :: AltWire -> f -> Bool
isRightWire (RightWire _) _ = True
isRightWire _ _ = False

isOutWire :: AltWire -> f -> Bool
isOutWire (OutWire _) _ = True
isOutWire _ _ = False

isInWire :: AltWire -> f -> Bool
isInWire (InWire _) _ = True
isInWire _ _ = False

lookupWire :: AltWire -> Assignment f -> Maybe f
lookupWire w Assignment {..} = case w of
  LeftWire i -> Map.lookup i assignmentLeft
  RightWire i -> Map.lookup i assignmentRight
  OutWire i -> Map.lookup i assignmentOut
  InWire i -> Map.lookup i assignmentIn

updateWire :: AltWire -> f -> Assignment f -> Assignment f
updateWire (LeftWire i) x assign =
  assign {assignmentLeft = Map.insert i x (assignmentLeft assign)}
updateWire (RightWire i) x assign =
  assign {assignmentRight = Map.insert i x (assignmentRight assign)}
updateWire (OutWire i) x assign =
  assign {assignmentOut = Map.insert i x (assignmentOut assign)}
updateWire (InWire i) x assign =
  assign {assignmentIn = Map.insert i x (assignmentIn assign)}

inputToAssignment :: Map Int f -> Assignment f
inputToAssignment inps = Assignment
  { assignmentLeft = Map.empty,
    assignmentRight = Map.empty,
    assignmentOut = Map.empty,
    assignmentIn = inps
  }

-- This is slightly different from ArithmeticCircuit.evalGate in that
-- this one also assigns values to the left and right wires.
evalGate ::
  (Num f) =>
  -- | initial context
  Assignment f ->
  -- | gate
  Gate AltWire f ->
  -- | context after evaluation
  Assignment f
evalGate vars (Mul lhs rhs (OutWire gateNumber)) =
  let lval = evalAffineCircuit lookupWire vars lhs
      rval = evalAffineCircuit lookupWire vars rhs
      res = lval * rval
   in updateWire (LeftWire gateNumber) lval
        $ updateWire (RightWire gateNumber) rval
        $ updateWire (OutWire gateNumber) res vars
evalGate _ _ = panic "evalGate: gate malformed"

evalCircuit ::
  Num f =>
  -- | circuit to evaluate
  AltArithCircuit f ->
  -- | initial context (containing input variables)
  Assignment f ->
  -- | input and output variables
  Assignment f
evalCircuit (AltArithCircuit gates) vars =
  foldl' evalGate vars gates

checkConstraints :: (Num f, Eq f) => GateConstraint AltWire f -> Assignment f -> Bool
checkConstraints (GateConstraint constraintL constraintR constraintMul) assign =
  and
    [ checkLinearConstraint constraintL assign,
      checkLinearConstraint constraintR assign,
      checkMulConstraint constraintMul assign
    ]

checkLinearConstraint ::
  (Num f, Eq f) =>
  LinearConstraint f ->
  Assignment f ->
  Bool
checkLinearConstraint LinearConstraint {..} Assignment {..} =
  lcWeightsLeft `dotProduct` assignmentLeft
    + lcWeightsRight `dotProduct` assignmentRight
    + lcWeightsOut `dotProduct` assignmentOut
    == lcWeightsIn `dotProduct` assignmentIn + lcConstant

checkMulConstraint ::
  (Num f, Eq f) =>
  MulConstraint AltWire ->
  Assignment f ->
  Bool
checkMulConstraint (MulConstraint l r o) vars = fromMaybe False $ do
  lval <- lookupWire l vars
  rval <- lookupWire r vars
  oval <- lookupWire o vars
  pure $ lval * rval == oval

-- | Generate constraints for a single multiplication gate
gateToConstraints :: Num f => Gate AltWire f -> GateConstraint AltWire f
gateToConstraints (Mul lhs rhs (OutWire gateNumber)) =
  let affineMapLeft = affineCircuitToAffineMap lhs
      affineMapRight = affineCircuitToAffineMap rhs
   in GateConstraint
        { gcLinearConstraintLeft =
            updateConstraint 1 (affineMapToLinearConstraint affineMapLeft) (LeftWire gateNumber),
          gcLinearConstraintRight =
            updateConstraint 1 (affineMapToLinearConstraint affineMapRight) (RightWire gateNumber),
          gcMulConstraint =
            MulConstraint (LeftWire gateNumber) (RightWire gateNumber) (OutWire gateNumber)
        }
gateToConstraints _ = panic "gateToConstraints: gate malformed"

-- spits out constraints "in reverse"
circuitToConstraints :: Num f => AltArithCircuit f -> [GateConstraint AltWire f]
circuitToConstraints (AltArithCircuit gates) =
  foldl' (\cs gate -> gateToConstraints gate : cs) [] gates

-- XXX: migrate example out of core library before release

---------------------------------------------------------
-- Example of an arithmetic circuit with a single gate
---------------------------------------------------------

-- (v0 + v1) * (v2 + 10)
exampleGate :: Num f => Gate AltWire f
exampleGate = Mul
  { mulLeft = Add (Var $ InWire 0) (Var $ InWire 1),
    mulRight = Add (Var $ InWire 2) (ConstGate 10),
    mulOutput = OutWire 0
  }

exampleEqns :: Num f => LinearConstraint f
exampleEqns = LinearConstraint
  { lcWeightsLeft = Map.fromList [(0, 1)],
    lcWeightsRight = Map.fromList [(0, 0)],
    lcWeightsOut = Map.fromList [(0, 0)],
    lcWeightsIn = Map.fromList [(0, 1)],
    lcConstant = 5
  }

exampleAssignment :: Num f => [f] -> Assignment f
exampleAssignment [v0, v1, v2] = Assignment
  { assignmentLeft = Map.fromList [(0, v0 + v1)],
    assignmentRight = Map.fromList [(0, v2 + 10)],
    assignmentOut = Map.fromList [(0, (v0 + v1) * (v2 + 10))],
    assignmentIn = Map.fromList [(0, v0), (1, v1), (2, v2)]
  }
exampleAssignment _ = panic "Invalid inputs for this example"

---------------------------------------------------------
-- Example of an arithmetic circuit with multiple gates
-- (Example from BCC 16. Appendix A. Efficient zero-knowledge
-- arguments for arithmetic circuits in the discrete log setting.)
---------------------------------------------------------

exampleMultiGates :: Num f => [Gate AltWire f]
exampleMultiGates =
  [ Mul
      { mulLeft = Var $ InWire 0,
        mulRight = Var $ InWire 1,
        mulOutput = OutWire 0
      },
    Mul
      { mulLeft = Var $ InWire 2,
        mulRight = Var $ InWire 3,
        mulOutput = OutWire 1
      },
    Mul
      { mulLeft = Var $ InWire 4,
        mulRight = Var $ InWire 5,
        mulOutput = OutWire 2
      },
    Mul
      { mulLeft = Var $ OutWire 0,
        mulRight = Var $ OutWire 1,
        mulOutput = OutWire 3
      },
    Mul
      { mulLeft = ScalarMul 4 (Var $ OutWire 2),
        mulRight = Add (ScalarMul 4 (Var $ OutWire 2)) (Var $ OutWire 3),
        mulOutput = OutWire 4
      },
    Mul
      { mulLeft = Var $ OutWire 3,
        mulRight = Add (ScalarMul 4 (Var $ OutWire 2)) (Var $ OutWire 3),
        mulOutput = OutWire 5
      }
  ]

exampleMultiAssignmentInitial :: [f] -> Assignment f
exampleMultiAssignmentInitial vs = Assignment
  { assignmentLeft = Map.empty,
    assignmentRight = Map.empty,
    assignmentOut = Map.empty,
    assignmentIn = Map.fromList (zip [0 ..] vs)
  }

--------------------------------------------------------
-- Bulletproofs arithmetic circuits conversion utils
--------------------------------------------------------

altToBulletproofsAssignment :: Num f => Int -> Assignment f -> Bulletproofs.Assignment f
altToBulletproofsAssignment n Assignment {..} =
  Bulletproofs.Assignment aL aR aO
  where
    aL = (\i -> fromMaybe 0 (Map.lookup i assignmentLeft)) <$> [0 .. n - 1]
    aR = (\i -> fromMaybe 0 (Map.lookup i assignmentRight)) <$> [0 .. n - 1]
    aO = (\i -> fromMaybe 0 (Map.lookup i assignmentOut)) <$> [0 .. n - 1]

altToBulletproofsCircuit :: forall f. Num f => AltArithCircuit f -> Bulletproofs.ArithCircuit f
altToBulletproofsCircuit (circuitToConstraints -> constraints) =
  Bulletproofs.ArithCircuit
    { weights = Bulletproofs.GateWeights wL wR wO,
      commitmentWeights = wV,
      cs = cs
    }
  where
    wL = foldl' (buildMatrix lcWeightsLeft (numberOfGates - 1)) [] constraints
    wR = foldl' (buildMatrix lcWeightsRight (numberOfGates - 1)) [] constraints
    wO = foldl' (buildMatrix lcWeightsOut (numberOfGates - 1)) [] constraints
    wV = foldl' (buildMatrix lcWeightsIn (m - 1)) [] constraints
    cs = foldl' (buildVector lcConstant) [] constraints
    numberOfGates = length constraints
    m = foldl' countWeigths 0 constraints
    buildVector :: (LinearConstraint f -> f) -> [f] -> GateConstraint AltWire f -> [f]
    buildVector f acc c = lConstraints : rConstraints : acc
      where
        lConstraints = f $ gcLinearConstraintLeft c
        rConstraints = f $ gcLinearConstraintRight c
    buildMatrix :: (LinearConstraint f -> Map Int f) -> Int -> [[f]] -> GateConstraint AltWire f -> [[f]]
    buildMatrix f n acc c = lConstraintsList : rConstraintsList : acc
      where
        lConstraints = f $ gcLinearConstraintLeft c
        lConstraintsList = (\i -> fromMaybe 0 (Map.lookup i lConstraints)) <$> [0 .. n]
        rConstraints = f $ gcLinearConstraintRight c
        rConstraintsList = (\i -> fromMaybe 0 (Map.lookup i rConstraints)) <$> [0 .. n]

countWeigths :: Int -> GateConstraint AltWire f -> Int
countWeigths acc c =
  acc
    + Map.size (lcWeightsIn $ gcLinearConstraintLeft c)
    + Map.size (lcWeightsIn $ gcLinearConstraintRight c)

calculateMatrixSizes :: (Num f) => AltArithCircuit f -> (Int, Int)
calculateMatrixSizes altCircuit = (m, n)
  where
    constraints = circuitToConstraints altCircuit
    n = fromIntegral $ length constraints
    m = foldl' countWeigths 0 constraints

data SetupProof f p
  = SetupProof
      { assignment :: Bulletproofs.Assignment f,
        pedersens :: Pedersens f p,
        circuit :: Bulletproofs.ArithCircuit f,
        witness :: Bulletproofs.ArithWitness f p,
        n :: Int,
        m :: Int
      }
  deriving (Show, Generic, NFData)

data Pedersens f p
  = Pedersens
      { vs :: [f],
        vBlindings :: [f],
        vCommitments :: [p]
      }
  deriving (Show, Generic, NFData)

computePedersens :: (MonadRandom m) => Int -> Int -> m (Pedersens Fr PA)
computePedersens n m = do
  vs <- replicateM m (fromInteger <$> getRandomR (0, 2 ^ n - 1))
  vBlindings <- replicateM m (fromInteger <$> getRandomR (0, 2 ^ n - 1))
  let vCommitments = zipWith commit vs vBlindings
  pure Pedersens
    { vs = vs,
      vBlindings = vBlindings,
      vCommitments = vCommitments
    }

computeBulletproofsAssignment :: AltArithCircuit Fr -> [Fr] -> Int -> Bulletproofs.Assignment Fr
computeBulletproofsAssignment altCircuit vs n =
  altToBulletproofsAssignment (fromIntegral n) altAssignment
  where
    altAssignment = evalCircuit altCircuit (exampleMultiAssignmentInitial vs)

setupProof :: (MonadRandom m) => AltArithCircuit Fr -> m (SetupProof Fr PA)
setupProof (transformInputs -> altCircuit) = do
  let (m, n) = calculateMatrixSizes altCircuit
      bulletproofsCircuit = altToBulletproofsCircuit altCircuit
  pedersens@Pedersens {..} <- computePedersens n m
  let assignment = computeBulletproofsAssignment altCircuit vs n
  let arithWitness = Bulletproofs.ArithWitness assignment vCommitments vBlindings
  pure SetupProof
    { assignment = assignment,
      pedersens = pedersens,
      circuit = bulletproofsCircuit,
      witness = arithWitness,
      n = n,
      m = m
    }
