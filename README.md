<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

# Arithmetic Circuits

An *arithmetic circuit* is a low-level representation of a program that consists
of gates computing arithmetic operations of addition and multiplication, with
wires connecting the gates.

This form allows us to express arbitrarily complex programs with a set of
*private inputs* and *public inputs* whose execution can be publicly verified
without revealing the private inputs. This construction relies on recent
advances in zero-knowledge proving systems such as Groth16, Pinnochio, and
Bulletproofs.

This library presents a low-level interface for building zkSNARK proving systems
from higher-level compilers. This system depends on the following cryptographic
dependenices.

* [galois-field](https://www.github.com/adjoint-io/galois-field) - Finite field
  arithmetic
* [galois-fft](https://www.github.com/adjoint-io/galois-fft) - Finite field
  polynomial arithmetic based on fast Fourier transforms
* [elliptic-curve](https://www.github.com/adjoint-io/elliptic-curve) - Elliptic
  curve operations
* [bulletproofs](https://www.github.com/adjoint-io/bulletproofs) - Bulletproofs
  proof system
* [arithmoi](https://www.github.com/adjoint-io/arithmoi) - Number theory
  operations
* [semirings](https://www.github.com/adjoint-io/semirings) - Algebraic semirings
* [poly](https://www.github.com/adjoint-io/poly) - Efficient polynomial
  arithmetic

## Theory

### Galois Fields

This library can build proof systems polymorphically over a variety of pairing
friendly curves. By default we use the [BN254](https://github.com/adjoint-io/elliptic-curve/blob/master/src/Data/Curve/Weierstrass/BN254.hs)
with an efficient implementation of the optimal Ate pairing.

The Barreto-Naehrig (BN) family of curves achieve high security and efficiency
with pairings due to an optimum embedding degree and high 2-adicity.. We have
implemented the optimal Ate pairing over the BN254 curve we define <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/> and <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/>
as:

* <img src="/tex/f041cced24931de6e6fbe77cf7fdc920.svg?invert_in_darkmode&sanitize=true" align=middle width=221.83192679999993pt height=26.76175259999998pt/>
* <img src="/tex/c0ea121dfdf29950f20f29c86635f197.svg?invert_in_darkmode&sanitize=true" align=middle width=221.77679534999993pt height=26.76175259999998pt/>
* <img src="/tex/71d71cc186d8238f79d664af6a48289f.svg?invert_in_darkmode&sanitize=true" align=middle width=184.01870685pt height=21.18721440000001pt/>

The tower of finite fields we work with is defined as:

* <img src="/tex/66eff114f6c4385dd25cca62457ad776.svg?invert_in_darkmode&sanitize=true" align=middle width=134.80114229999998pt height=26.76175259999998pt/>
* <img src="/tex/661afe19025c1f97ae61a41f91d325e9.svg?invert_in_darkmode&sanitize=true" align=middle width=181.79879189999997pt height=26.76175259999998pt/>
* <img src="/tex/ad84179c73fb68d93d2a147cce2cb19c.svg?invert_in_darkmode&sanitize=true" align=middle width=152.75024985pt height=26.76175259999998pt/>

### Arithmetic circuits

<p align="center">
<img src="./.assets/circuit.png" alt="Arithmetic Circuit" height=300 align="left" />
</p>

An arithmetic circuit over a finite field is a directed acyclic graph with gates
as vertices and wires and edges. It consists of a list of multiplication gates
together with a set of linear consistency equations relating the inputs and
outputs of the gates.

Let <img src="/tex/2d4c6ac334688c42fb4089749e372345.svg?invert_in_darkmode&sanitize=true" align=middle width=10.045686749999991pt height=22.648391699999998pt/> be a finite field and <img src="/tex/c8a92e9bd23d2e4841e72114a69462d7.svg?invert_in_darkmode&sanitize=true" align=middle width=124.1115777pt height=27.91243950000002pt/> a map that takes <img src="/tex/5e27bca98285ab8eccf4d53506baeaec.svg?invert_in_darkmode&sanitize=true" align=middle width=39.42918209999999pt height=22.831056599999986pt/>
arguments as inputs from <img src="/tex/2d4c6ac334688c42fb4089749e372345.svg?invert_in_darkmode&sanitize=true" align=middle width=10.045686749999991pt height=22.648391699999998pt/> and outputs l elements in <img src="/tex/2d4c6ac334688c42fb4089749e372345.svg?invert_in_darkmode&sanitize=true" align=middle width=10.045686749999991pt height=22.648391699999998pt/>. The function C is an arithmetic circuit if the
value of the inputs that pass through wires to gates are only manipulated according to arithmetic operations + or x (allowing
constant gates).

Let <img src="/tex/55a049b8f161ae7cfeb0197d75aff967.svg?invert_in_darkmode&sanitize=true" align=middle width=9.86687624999999pt height=14.15524440000002pt/>, <img src="/tex/2ad9d098b937e46f9f58968551adac57.svg?invert_in_darkmode&sanitize=true" align=middle width=9.47111549999999pt height=22.831056599999986pt/>, <img src="/tex/2f2322dff5bde89c37bcae4116fe20a8.svg?invert_in_darkmode&sanitize=true" align=middle width=5.2283516999999895pt height=22.831056599999986pt/> respectively denote the input, witness and output size and
<img src="/tex/dc35769e37858254d0d77fab2d83bcf4.svg?invert_in_darkmode&sanitize=true" align=middle width=114.45175665pt height=24.65753399999998pt/> be the number of all inputs and outputs of the circuit
A tuple <img src="/tex/2c4cf6568afbabb029a579215dfa6e3e.svg?invert_in_darkmode&sanitize=true" align=middle width=120.09967859999999pt height=27.6567522pt/>, is said to be a valid
assignment for an arithmetic circuit C if <img src="/tex/86c8263cd0afa711b888c85bcb5b03a3.svg?invert_in_darkmode&sanitize=true" align=middle width=241.74771059999995pt height=24.65753399999998pt/>.


### Quadratic Arithmetic Programs (QAP)

QAPs are encodings of arithmetic circuits that allow the prover to construct a
proof of knowledge of a valid assignment <img src="/tex/6e1ad13b9c0521871bb453942700c519.svg?invert_in_darkmode&sanitize=true" align=middle width=120.09967859999999pt height=27.6567522pt/> for a given
circuit <img src="/tex/9b325b9e31e85137d1de765f43c0f8bc.svg?invert_in_darkmode&sanitize=true" align=middle width=12.92464304999999pt height=22.465723500000017pt/>.

A quadratic arithmetic program (QAP) <img src="/tex/9000cff3d46536c190fabb076ebe7cbb.svg?invert_in_darkmode&sanitize=true" align=middle width=38.70549539999999pt height=24.65753399999998pt/> contains three sets of polynomials in
<img src="/tex/a420c52aa24a502d60aef830b3b45f9f.svg?invert_in_darkmode&sanitize=true" align=middle width=28.57312259999999pt height=24.65753399999998pt/>:

<img src="/tex/cf792d8b490521d817a643a4adea6f28.svg?invert_in_darkmode&sanitize=true" align=middle width=184.37011065pt height=24.65753399999998pt/>, <img src="/tex/258504deb4909bd9a3058fffbdb20262.svg?invert_in_darkmode&sanitize=true" align=middle width=185.47456784999997pt height=24.65753399999998pt/>, <img src="/tex/36b962f85e4e4de2a88919a5e00acbe1.svg?invert_in_darkmode&sanitize=true" align=middle width=184.38600014999997pt height=24.65753399999998pt/>

and a target polynomial <img src="/tex/083da1124b81d709f20f2575ae9138c3.svg?invert_in_darkmode&sanitize=true" align=middle width=34.06973294999999pt height=24.65753399999998pt/>.

In this setting, an assignment <img src="/tex/d1dd493c98f06e9ef29b5fdc411e29f8.svg?invert_in_darkmode&sanitize=true" align=middle width=78.31669229999999pt height=24.65753399999998pt/> is valid for a circuit <img src="/tex/9b325b9e31e85137d1de765f43c0f8bc.svg?invert_in_darkmode&sanitize=true" align=middle width=12.92464304999999pt height=22.465723500000017pt/> if and
only if the target polynomial <img src="/tex/083da1124b81d709f20f2575ae9138c3.svg?invert_in_darkmode&sanitize=true" align=middle width=34.06973294999999pt height=24.65753399999998pt/> divides the polynomial:

<img src="/tex/d6b98fe1452c7c16cfd1ad84a2e7331b.svg?invert_in_darkmode&sanitize=true" align=middle width=613.0190187pt height=26.438629799999987pt/>

Logical circuits can be written in terms of the addition, multiplication and
negation operations.

* <img src="/tex/05487edead6a212f2c6b0eb882e301bf.svg?invert_in_darkmode&sanitize=true" align=middle width=130.41681344999998pt height=24.65753399999998pt/>
* <img src="/tex/26affbf877c9c00bcf2d4f78f88cedbf.svg?invert_in_darkmode&sanitize=true" align=middle width=120.27647609999997pt height=24.65753399999998pt/>
* <img src="/tex/d42ecdc0230bb52bdc11f68c73872136.svg?invert_in_darkmode&sanitize=true" align=middle width=153.63599129999997pt height=24.65753399999998pt/>
* <img src="/tex/93695159b317a047750512fe6c997521.svg?invert_in_darkmode&sanitize=true" align=middle width=179.85417449999997pt height=24.65753399999998pt/>
* <img src="/tex/57e4d6a592fb9f580ff984df8690de97.svg?invert_in_darkmode&sanitize=true" align=middle width=221.99032019999996pt height=24.65753399999998pt/>

## Circuit Builder Monad

TODO

## Example

The following example represents the image of the arithmetic circuit
[above](#arithmetic-circuits-1). We'll use the library
[pairing](https://www.github.com/adjoint-io/pairing) that provides a field of
points of the BN254 curve and precomputes primitive roots of unity for binary
powers that divide <img src="/tex/580e7a6446bf50562e34247c545883a2.svg?invert_in_darkmode&sanitize=true" align=middle width=36.18335654999999pt height=21.18721440000001pt/>.

```haskell
{-# LANGUAGE DataKinds #-}
import Protolude

import qualified Data.Map as Map
import Data.Pairing.BN254 (Fr, getRootOfUnity)

import Circuit.Arithmetic (Gate(..), ArithCircuit(..), generateRoots)
import Circuit.Affine (AffineCircuit(..))
import Fresh (evalFresh, fresh)
import QAP (arithCircuitToQAP, arithCircuitToQAPFFT, createPolynomials, createPolynomialsFFT)

program :: ArithCircuit Fr
program = ArithCircuit
  [ Mul (Var (InputWire 0)) (Var (InputWire 1)) (IntermediateWire 0)
  , Mul (IntermediateWire 0)(Add (Var (InputWire 0)) (Var (InputWire 2))) (OutputWire 0)
  ]
```

We need to generate the roots of the circuit to construct polynomials <img src="/tex/083da1124b81d709f20f2575ae9138c3.svg?invert_in_darkmode&sanitize=true" align=middle width=34.06973294999999pt height=24.65753399999998pt/> and
<img src="/tex/52be0087c9da1f0683ccc50761e8bcab.svg?invert_in_darkmode&sanitize=true" align=middle width=35.01719264999999pt height=24.65753399999998pt/> that satisfy the divisibility property and encode the circuit to a QAP to
allow the prover to construct a proof of a valid assignment.

```haskell
roots :: [[Fr]]
roots = evalFresh <img src="/tex/03928532e020cc2450c9e305aa51e636.svg?invert_in_darkmode&sanitize=true" align=middle width=270.32995935pt height=24.65753399999998pt/>> fresh) program

qap :: QAP Fr
qap = arithCircuitToQAPFFT getRootOfUnity roots program
```

*Note*: If a function to find the primitive roots of unity of the prime field
<img src="/tex/2d4c6ac334688c42fb4089749e372345.svg?invert_in_darkmode&sanitize=true" align=middle width=10.045686749999991pt height=22.648391699999998pt/> used cannot be given, a slower conversion to a QAP can be used.

There are three input wires to this arithmetic circuit. A valid input would be:

```haskell
input :: Map.Map Int Fr
input = Map.fromList [(0, 7), (1, 5), (2, 4)]
```

A prover can now generate a valid assignment:

```haskell
assignment :: QAPSet Fr
assignment = generateAssignment program input
```

The verifier can check the divisibility property of <img src="/tex/52be0087c9da1f0683ccc50761e8bcab.svg?invert_in_darkmode&sanitize=true" align=middle width=35.01719264999999pt height=24.65753399999998pt/> by <img src="/tex/083da1124b81d709f20f2575ae9138c3.svg?invert_in_darkmode&sanitize=true" align=middle width=34.06973294999999pt height=24.65753399999998pt/> for the
given QAP.

```haskell
main :: IO ()
main = do
  pure $ verifyAssigment qap assignment
```

## Disclaimer

This is experimental code meant for research-grade projects only. Please do not
use this code in production until it has matured significantly.

## License

```
Copyright (c) 2017-2020 Adjoint Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.
```
