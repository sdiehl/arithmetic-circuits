<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

# arithmetic-circuits

TODO

## Theory

### Arithmetic circuits

An arithmetic circuit over a finite field is a
directed acyclic graph with gates as vertices and wires and edges. It consists of a list of multiplication gates together with a set of linear
consistency equations relating the inputs and outputs of the gates.

Let F be a finite field and C: F^n x F^h -> F^l a map that takes n+h
arguments as inputs from F and outputs l elements in F. The function C is an arithmetic circuit if the
value of the inputs that pass through wires to gates are only manipulated according to arithmetic operations + or x (allowing
constant gates).

Let n, h, l respectively denote the input, witness and output size and
N = (n+h)+l be the number of all inputs and outputs of the circuit
A tuple (a_1, ..., a_N) \in F^N, is said to be a valid
assignment for an arithmetic circuit C if C(a_1,...,a_{n+h}) = (a_{n+h+1}, ..., a_N).


### Quadratic Arithmetic Programs (QAP)

QAPs are encodings of arithmetic circuits that allow the prover to construct a
proof of knowledge of a valid assignment (a_1,...,a_N) \in F^N for a given
circuit C.

A quadratic arithmetic program (QAP) Q(C) contains three sets of polynomials in F[x]:
A={A_k(x) : k \in {0..m}}, B={B_k(x) : k \in {0..m}} and C={C_k(x) : k \in {0..m}},
and a target polynomial t(x).

In this setting, an assignment (a_1,...,a_N) is valid for a circuit C if and only if the target
polynomial t(x) divides the polynomial:
P(x) = (A_0(x) + \sum_{k=1}^m a_k A_k(x)) (B_0(x) + \sum_{k=1}^m a_k B_k(x)) - (C_0(x) + \sum_{k=1}^m a_k C_k(x))

## Example

TODO

```haskell
import Protolude
```

## Disclaimer

This is experimental code meant for research-grade projects only. Please do not
use this code in production until it has matured significantly.

## License

```
Copyright (c) 2017-2019 Adjoint Inc.

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
