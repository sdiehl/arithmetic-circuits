{-# LANGUAGE NoImplicitPrelude #-}

module Circuit (benchmarks) where

import Protolude

import           Circuit.Affine
import           Circuit.Arithmetic
import           Criterion.Main
import           Data.Curve.Weierstrass.BN254 (Fr)
import qualified Data.Map                     as Map
import           Data.Pairing.BN254           (getRootOfUnity)
import           Fresh
import           QAP


program :: ArithCircuit Fr
program = ArithCircuit
  [ Mul (Var (InputWire 0)) (Var (InputWire 1)) (IntermediateWire 0)
  , Mul (Var (IntermediateWire 0))(Add (Var (InputWire 0)) (Var (InputWire 2))) (OutputWire 0)
  ]

input :: Map.Map Int Fr
input = Map.fromList [(0, 7), (1, 5), (2, 4)]

benchmarks :: [Benchmark]
benchmarks
  = [ bench "evaluating circuit"
    $ whnf (evalArithCircuit lookupAtWire updateAtWire program) (initialQapSet input)
    , bench "creating QAP (no interpolation)"
    $ nf (\c -> arithCircuitToGenQAP (evalFresh $ generateRoots (fromIntegral <$> fresh) c) c) program
    , bench "creating QAP (fast interpolation)"
    $ nf (\c -> arithCircuitToQAPFFT getRootOfUnity (evalFresh $ generateRoots (fromIntegral <$> fresh) c) c) program
    , bench "creating QAP (slow interpolation)"
    $ nf (\c -> arithCircuitToQAP (evalFresh $ generateRoots (fromIntegral <$> fresh) c) c) program
    ]

