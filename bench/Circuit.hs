{-# LANGUAGE NoImplicitPrelude #-}

module Circuit (benchmarks) where

import Protolude

import           Circuit.Arithmetic
import           Circuit.Expr                 hiding (fresh)
import           Criterion.Main
import           Data.Curve.Weierstrass.BN254 (Fr)
import           Data.Field.Galois            (PrimeField(..))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Pairing.BN254           (getRootOfUnity)
import           Fresh
import           QAP
import           Reference.Cubehash           (additionCircuit, buildInputs)

numBits :: Int
numBits = 16

createCircuit :: Int -> ArithCircuit Fr
createCircuit = execCircuitBuilder . additionCircuit . buildInputs

prepareCircuit :: Int -> ([Wire], ArithCircuit Fr)
prepareCircuit = runCircuitBuilder . additionCircuit . buildInputs

evalCircuit :: ([Wire], ArithCircuit Fr) -> (Integer, Integer) -> Integer
evalCircuit (circOutps, circuit) (inpl, inpr) = outp
  where
    inpMap :: Map Int Fr
    inpMap = Map.fromList . zip [0..] $ intToFrBits inpl ++ intToFrBits inpr

    outp :: Integer
    outp = frBitsToInt outputs

    outputs :: [Fr]
    outputs = reverse $ mapMaybe (`lookupAtWire` outpMap) circOutps

    outpMap :: QapSet Fr
    outpMap = evalArithCircuit lookupAtWire updateAtWire circuit (initialQapSet inpMap)

    -- | little endian
    intToBits :: Integer -> [Integer]
    intToBits = unfoldr op
      where
        op b | b <= 0 = Nothing
             | otherwise = Just . swap $ b `quotRem` 2

    bitsToInt :: [Integer] -> Integer
    bitsToInt = foldr (\b r -> b + 2 * r) 0

    intToFrBits :: Integer -> [Fr]
    intToFrBits = map (fromIntegral . toInteger) . padBits . intToBits

    frBitsToInt :: [Fr] -> Integer
    frBitsToInt = bitsToInt . map fromP

    padBits :: [Integer] -> [Integer]
    padBits xs
      = xs ++ replicate (numBits - length xs) 0

benchmarks :: [Benchmark]
benchmarks
  = [ bgroup (show numBits <> " bit addition circuit")
      [ bench "creating circuit"
      $ nf createCircuit numBits
      , env (pure $ prepareCircuit numBits) $ \prep ->
        bench "evaluating circuit"
      $ whnf (evalCircuit prep) (1234567890, 2345678901)
      , env (pure $ createCircuit numBits) $ \circ ->
        bench "creating QAP (no interpolation)"
      $ nf (\c -> arithCircuitToGenQAP (evalFresh $ generateRoots (fromIntegral <$> fresh) c) c) circ
      , env (pure $ createCircuit numBits) $ \circ ->
        bench "creating QAP (fast interpolation)"
      $ nf (\c -> createPolynomials getRootOfUnity $ arithCircuitToGenQAP (evalFresh $ generateRoots (fromIntegral <$> fresh) c) c) circ
      , env (pure $ createCircuit numBits) $ \circ ->
        bench "creating QAP (slow interpolation)"
      $ nf (\c -> arithCircuitToQAP (evalFresh $ generateRoots (fromIntegral <$> fresh) c) c) circ
      ]
    ]
