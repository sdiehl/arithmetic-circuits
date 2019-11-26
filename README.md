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
* <img src="/tex/a45a5d659b15986b26d1aeba7c808734.svg?invert_in_darkmode&sanitize=true" align=middle width=173.72718329999998pt height=24.65753399999998pt/>
* <img src="/tex/e7994598838dc8af6fb203d13bc3690c.svg?invert_in_darkmode&sanitize=true" align=middle width=240.1277472pt height=24.65753399999998pt/>
* <img src="/tex/57e4d6a592fb9f580ff984df8690de97.svg?invert_in_darkmode&sanitize=true" align=middle width=221.99032019999996pt height=24.65753399999998pt/>

## DSL and Circuit Builder Monad

Any arithmetic circuit can be built using a domain specific language to
construct circuits that lives inside [Lang.hs](src/Circuit/Lang.hs).

```haskell ignore
type ExprM f a = State (ArithCircuit f, Int) a
execCircuitBuilder :: ExprM f a -> ArithCircuit f
```

```haskell ignore
-- | Binary arithmetic operations
add, sub, mul :: Expr Wire f f -> Expr Wire f f -> Expr Wire f f
```

```haskell ignore
-- | Binary logic operations
-- Have to use underscore or similar to avoid shadowing @and@ and @or@
-- from Prelude/Protolude.
and_, or_, xor_ :: Expr Wire f Bool -> Expr Wire f Bool -> Expr Wire f Bool
```

```haskell ignore
-- | Negate expression
not_ :: Expr Wire f Bool -> Expr Wire f Bool
```

```haskell ignore
-- | Compare two expressions
eq :: Expr Wire f f -> Expr Wire f f -> Expr Wire f Bool
```

```haskell ignore
-- | Convert wire to expression
deref :: Wire -> Expr Wire f f
```

```haskell ignore
-- | Return compilation of expression into an intermediate wire
e :: Num f => Expr Wire f f -> ExprM f Wire
```

```haskell ignore
-- | Conditional statement on expressions
cond :: Expr Wire f Bool -> Expr Wire f ty -> Expr Wire f ty -> Expr Wire f ty
```

```haskell ignore
-- | Return compilation of expression into an output wire
ret :: Num f => Expr Wire f f -> ExprM f Wire
```

The following program represents the image of the
arithmetic circuit [above](#arithmetic-circuits-1).

```haskell ignore
program :: ArithCircuit Fr
program = execCircuitBuilder <img src="/tex/4fc182d9ca10181f85444d64991b4f62.svg?invert_in_darkmode&sanitize=true" align=middle width=124.01565329999998pt height=22.831056599999986pt/>> input
  i1 <- deref <<img src="/tex/1c1d4fa3482507a0a0ce9485579cc4a9.svg?invert_in_darkmode&sanitize=true" align=middle width=163.99021154999997pt height=22.831056599999986pt/>> input
  let r0 = mul i0 i1
      r1 = mul r0 (add i0 i2)
  ret r1
```

The output of an arithmetic circuit can be converted to a DOT graph and display
it as a graph.

```haskell ignore
dotOutput :: Text
dotOutput = arithCircuitToDot <img src="/tex/79bf90d763b01295f18042a947d15f47.svg?invert_in_darkmode&sanitize=true" align=middle width=700.5028733999999pt height=164.20092150000002pt/>r - 1<img src="/tex/e669a12254dafeeb34eec7c07244ce05.svg?invert_in_darkmode&sanitize=true" align=middle width=700.27457115pt height=203.6529759pt/> do
  i0 <- deref <<img src="/tex/465343d6773512ce7812f866f58faf28.svg?invert_in_darkmode&sanitize=true" align=middle width=163.99021154999997pt height=22.831056599999986pt/>> input
  i2 <- deref <<img src="/tex/7bfea768905ffffe4fc4a7ad428c05b2.svg?invert_in_darkmode&sanitize=true" align=middle width=490.86891149999997pt height=45.84475500000001pt/>T(x)<img src="/tex/b2584d6517f9c72bcd800016d8d1fa0d.svg?invert_in_darkmode&sanitize=true" align=middle width=27.11199479999999pt height=22.831056599999986pt/>P(x)<img src="/tex/3d688cfd3f6ea02dead75f511c40e5c0.svg?invert_in_darkmode&sanitize=true" align=middle width=902.7356893499999pt height=322.0091391pt/>P(x)<img src="/tex/2441df23627a504b2a4c6f5006893fd6.svg?invert_in_darkmode&sanitize=true" align=middle width=15.70402184999999pt height=22.831056599999986pt/>T(x)$ for the given circuit.

```haskell
main :: IO ()
main = do
  if verifyAssignment qap assignment
    then putText "Valid assignment"
    else putText "Invalid assignment"
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
