{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

-- | Definition of arithmetic circuits that only contain addition,
-- scalar multiplications and constant gates, along with its direct
-- evaluation and translation into affine maps.
module Circuit.Affine
  ( AffineCircuit (..),
    collectInputsAffine,
    mapVarsAffine,
    evalAffineCircuit,
    affineCircuitToAffineMap,
    evalAffineMap,
    dotProduct,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Protolude
import Text.PrettyPrint.Leijen.Text
  ( (<+>),
    Doc,
    Pretty (..),
    parens,
    text,
  )

-- | Arithmetic circuits without multiplication, i.e. circuits
-- describe affine transformations.
data AffineCircuit i f
  = Add (AffineCircuit i f) (AffineCircuit i f)
  | ScalarMul f (AffineCircuit i f)
  | ConstGate f
  | Var i
  deriving (Read, Eq, Show, Generic, NFData)

collectInputsAffine :: Ord i => AffineCircuit i f -> [i]
collectInputsAffine = \case
  Add l r -> collectInputsAffine l ++ collectInputsAffine r
  ScalarMul _ x -> collectInputsAffine x
  ConstGate _ -> []
  Var i -> [i]

instance (Pretty i, Show f) => Pretty (AffineCircuit i f) where
  pretty = prettyPrec 0
    where
      prettyPrec :: (Pretty i, Show f) => Int -> AffineCircuit i f -> Doc
      prettyPrec p e =
        case e of
          Var v ->
            pretty v
          ConstGate f ->
            text $ show f
          ScalarMul f e1 ->
            text (show f) <+> text "*" <+> parensPrec 7 p (prettyPrec p e1)
          Add e1 e2 ->
            parensPrec 6 p $
              prettyPrec 6 e1
                <+> text "+"
                <+> prettyPrec 6 e2

parensPrec :: Int -> Int -> Doc -> Doc
parensPrec opPrec p = if p > opPrec then parens else identity

-- | Apply mapping to variable names, i.e. rename variables. (Ideally
-- the mapping is injective.)
mapVarsAffine :: (i -> j) -> AffineCircuit i f -> AffineCircuit j f
mapVarsAffine f = \case
  Add l r -> Add (mapVarsAffine f l) (mapVarsAffine f r)
  ScalarMul s expr -> ScalarMul s $ mapVarsAffine f expr
  ConstGate c -> ConstGate c
  Var i -> Var $ f i

-- | Evaluate the arithmetic circuit without mul-gates on the given
-- input. Variable map is assumed to have all the variables referred
-- to in the circuit. Failed lookups are currently treated as 0.
evalAffineCircuit ::
  Num f =>
  -- | lookup function for variable mapping
  (i -> vars -> Maybe f) ->
  -- | variables
  vars ->
  -- | circuit to evaluate
  AffineCircuit i f ->
  f
evalAffineCircuit lookupVar vars = \case
  ConstGate f -> f
  Var i -> fromMaybe 0 $ lookupVar i vars
  Add l r -> evalAffineCircuit lookupVar vars l + evalAffineCircuit lookupVar vars r
  ScalarMul scalar expr -> evalAffineCircuit lookupVar vars expr * scalar

-- | Convert non-mul circuit to a vector representing the evaluation
-- function. We use a @Map@ to represent the potentially sparse vector.
affineCircuitToAffineMap ::
  (Num f, Ord i) =>
  -- | circuit to translate
  AffineCircuit i f ->
  -- | constant part and non-constant part
  (f, Map i f)
affineCircuitToAffineMap = \case
  Var i -> (0, Map.singleton i 1)
  Add l r -> (constLeft + constRight, Map.unionWith (+) vecLeft vecRight)
    where
      (constLeft, vecLeft) = affineCircuitToAffineMap l
      (constRight, vecRight) = affineCircuitToAffineMap r
  ScalarMul scalar expr -> (scalar * constExpr, fmap (scalar *) vecExpr)
    where
      (constExpr, vecExpr) = affineCircuitToAffineMap expr
  ConstGate f -> (f, Map.empty)

-- | Evaluating the affine map representing the arithmetic circuit
-- without mul-gates against inputs. If the input map does not have a
-- variable that is referred to in the affine map, then it is treated
-- as a 0.
evalAffineMap ::
  (Num f, Ord i) =>
  -- | program split into constant and non-constant part
  (f, Map i f) ->
  -- | input variables
  Map i f ->
  f
evalAffineMap (constPart, linearPart) input =
  constPart + dotProduct linearPart input

dotProduct :: (Num f, Ord i) => Map i f -> Map i f -> f
dotProduct inp comp =
  sum
    . Map.elems
    $ Map.mapWithKey (\ix c -> c * Map.findWithDefault 0 ix inp) comp
