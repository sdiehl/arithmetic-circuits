import Circuit.Arithmetic
import Circuit.Expr
import Circuit.Lang
import qualified Data.Map as Map
import Data.Pairing.BN254 (Fr, getRootOfUnity)
import Fresh (evalFresh, fresh)
import Protolude
import QAP

program :: ArithCircuit Fr
program =
  execCircuitBuilder
    ( do
        i0 <- fmap deref input
        i1 <- fmap deref input
        i2 <- fmap deref input
        let r0 = mul i0 i1
            r1 = mul r0 (add i0 i2)
        ret r1
    )

roots :: [[Fr]]
roots = evalFresh (generateRoots (fmap (fromIntegral . (+ 1)) fresh) program)

qap :: QAP Fr
qap = arithCircuitToQAPFFT getRootOfUnity roots program

inputs :: Map.Map Int Fr
inputs = Map.fromList [(0, 7), (1, 5), (2, 4)]

assignment :: QapSet Fr
assignment = generateAssignment program inputs

main :: IO ()
main =
  if verifyAssignment qap assignment
    then putText "Valid assignment"
    else putText "Invalid assignment"
