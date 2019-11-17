{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, ScopedTypeVariables #-}

-- | Definition of arithmetic circuits: one with a single
-- multiplication gate with affine inputs and another variant with an
-- arbitrary number of such gates.
module Circuit.Arithmetic
  ( Gate(..)
  , mapVarsGate
  , collectInputsGate
  , outputWires
  , ArithCircuit(..)
  , fetchVars
  , generateRoots
  , validArithCircuit
  , Wire(..)
  , evalGate
  , evalArithCircuit
  , unsplit
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text as PP (Pretty(..), hsep, list, parens, text,
                                           vcat)

import Circuit.Affine (AffineCircuit(..), collectInputsAffine,
                       evalAffineCircuit, mapVarsAffine)

-- | Wires are can be labeled in the ways given in this data type
data Wire = InputWire Int
          | IntermediateWire Int
          | OutputWire Int
  deriving (Show, Eq, Ord, Generic, NFData)

instance Pretty Wire where
  pretty (InputWire v) = text "input_" <> pretty v
  pretty (IntermediateWire v) = text "imm_" <> pretty v
  pretty (OutputWire v) = text "output_" <> pretty v

-- | An arithmetic circuit with a single multiplication gate.
data Gate i f
  = Mul
    { mulLeft :: AffineCircuit i f
    , mulRight :: AffineCircuit i f
    , mulOutput :: i
    }
  | Equal
    { eqInput :: i
    , eqMagic :: i
    , eqOutput :: i
    }
  | Split
    { splitInput :: i
    , splitOutputs :: [i]
    }
  deriving (Show, Generic, NFData)

collectInputsGate :: Ord i => Gate i f -> [i]
collectInputsGate  = \case
  Mul l r _ -> collectInputsAffine l ++ collectInputsAffine r
  _ -> panic "collectInputsGate: only supports mul gates"

-- | List output wires of a gate
outputWires :: Gate i f -> [i]
outputWires = \case
  Mul _ _ out -> [out]
  Equal _ _ out -> [out]
  Split _ outs -> outs

instance (Pretty i, Show f) => Pretty (Gate i f) where
  pretty (Mul l r o) = hsep
    [ pretty o
    , text ":="
    , parens (pretty l)
    , text "*"
    , parens (pretty r)
    ]
  pretty (Equal i _ o) = hsep
    [ pretty o
    , text ":="
    , pretty i
    , text "== 0 ? 0 : 1"
    ]
  pretty (Split inp outputs) = hsep
    [ PP.list (map pretty outputs)
    , text ":="
    , text "split"
    , pretty inp
    ]

-- | Apply mapping to variable names, i.e. rename variables. (Ideally
-- the mapping is injective.)
mapVarsGate :: (i -> j) -> Gate i f -> Gate j f
mapVarsGate f = \case
  Mul l r o -> Mul (mapVarsAffine f l) (mapVarsAffine f r) (f o)
  Equal i j o -> Equal (f i) (f j) (f o)
  Split i os -> Split (f i) (fmap f os)

-- | Evaluate a single gate
evalGate
  :: (Bits f, Fractional f)
  => (i -> vars -> Maybe f) -- ^ lookup a value at a wire
  -> (i -> f -> vars -> vars) -- ^ update a value at a wire
  -> vars -- ^ context before evaluation
  -> Gate i f -- ^ gate
  -> vars -- ^ context after evaluation
evalGate lookupVar updateVar vars gate
  = case gate of
      Mul l r outputWire
        -> let lval = evalAffineCircuit lookupVar vars l
               rval = evalAffineCircuit lookupVar vars r
               res = lval * rval
           in updateVar outputWire res vars
      Equal i m outputWire
        -> case lookupVar i vars of
             Nothing
               -> panic "evalGate: the impossible happened"
             Just inp
               -> let res = if inp == 0 then 0 else 1
                      mid = if inp == 0 then 0 else recip inp
                  in updateVar outputWire res
                     $ updateVar m mid vars
      Split i os
        -> case lookupVar i vars of
             Nothing
               -> panic "evalGate: the impossible happened"
             Just inp
               -> let bool2val True = 1
                      bool2val False = 0
                      setWire (ix, oldEnv) currentOut
                        = ( ix + 1
                          , updateVar currentOut (bool2val $ testBit inp ix) oldEnv
                          )
                   in snd . foldl setWire (0, vars) $ os

-- | A circuit is a list of multiplication gates along with their
-- output wire labels (which can be intermediate or actual outputs).
newtype ArithCircuit f = ArithCircuit [Gate Wire f]
  deriving (Show, Generic, NFData)

instance Show f => Pretty (ArithCircuit f) where
  pretty (ArithCircuit gs) = vcat . map pretty $ gs

-- | Check whether an arithmetic circuit does not refer to
-- intermediate wires before they are defined and whether output wires
-- are not used as input wires.
validArithCircuit
  :: ArithCircuit f -> Bool
validArithCircuit (ArithCircuit gates)
  = noRefsToUndefinedWires
  where
    noRefsToUndefinedWires
      = fst
        $ foldl (\(res, definedWires) gate
                   -> ( res
                        && all isNotInput (outputWires gate)
                        && all (validWire definedWires) (fetchVarsGate gate)
                      , outputWires gate ++ definedWires
                ))
                (True, [])
                gates

    isNotInput (InputWire _) = False
    isNotInput (OutputWire _) = True
    isNotInput (IntermediateWire _) = True

    validWire _ (InputWire _) = True
    validWire _ (OutputWire _) = False
    validWire definedWires i@(IntermediateWire _) = i `elem` definedWires

    fetchVarsGate (Mul l r _) = fetchVars l ++ fetchVars r
    fetchVarsGate (Equal i _ _) = [i] -- we can ignore the magic
                                      -- variable "m", as it is filled
                                      -- in when evaluating the circuit
    fetchVarsGate (Split i _) = [i]

fetchVars :: AffineCircuit Wire f -> [Wire]
fetchVars (Var i) = [i]
fetchVars (ConstGate _) = []
fetchVars (ScalarMul _ c) = fetchVars c
fetchVars (Add l r) = fetchVars l ++ fetchVars r

-- | Generate enough roots for a circuit
generateRoots
  :: Applicative m
  => m f -> ArithCircuit f -> m [[f]]
generateRoots _ (ArithCircuit [])
  = pure []
generateRoots takeRoot (ArithCircuit (gate:gates))
  = case gate of
      Mul {}
        -> (\r rs -> [r]:rs)
             <$> takeRoot
             <*> generateRoots takeRoot (ArithCircuit gates)
      Equal {}
        -> (\r0 r1 rs -> [r0,r1]:rs)
             <$> takeRoot
             <*> takeRoot
             <*> generateRoots takeRoot (ArithCircuit gates)
      Split _ outputs
        -> (\r0 rOutputs rRest -> (r0:rOutputs):rRest)
             <$> takeRoot
             <*> traverse (const takeRoot) outputs
             <*> generateRoots takeRoot (ArithCircuit gates)

-- | Evaluate an arithmetic circuit on a given environment containing
-- the inputs. Outputs the entire environment (outputs, intermediate
-- values and inputs).
evalArithCircuit
  :: forall f vars
   . (Bits f, Fractional f)
  => (Wire -> vars -> Maybe f) -- ^ lookup a value at a wire
  -> (Wire -> f -> vars -> vars) -- ^ update a value at a wire
  -> ArithCircuit f -- ^ circuit to evaluate
  -> vars -- ^ input variables
  -> vars -- ^ input and output variables
evalArithCircuit lookupVar updateVar (ArithCircuit gates) vars
  = foldl' (evalGate lookupVar updateVar) vars gates


-- | Turn a binary expansion back into a single value.
unsplit
  :: Num f
  => [Wire] -- ^ (binary) wires containing a binary expansion,
            -- small-endian
  -> AffineCircuit Wire f
unsplit = snd . foldl (\(ix, rest) wire -> (ix + (1 :: Integer), Add rest (ScalarMul (2^ix) (Var wire)))) (0, ConstGate 0)
