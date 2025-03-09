# GenRMC - Generic Relational Model Checking

A Haskell library for relational programming with pluggable state-set representations and constraint systems. Intended to demonstrate a minimal, usable basis for relational programming that's generic over things like search strategy and state-superposition representation. A generalization of the relational machine calculus.

- [Relational Machine Calculus](https://arxiv.org/abs/2405.10801)

## Overview

GenRMC implements a generic framework for relational programming. It allows the implementation of:

- Different types of state representations
- Different constraint systems
- Generic operational semantics for relational programs

The framework is based on a core syntax for relational programs that includes:

- `Star` - success/identity
- `Comp p q` - sequential composition
- `Or p q` - non-deterministic choice
- `Ex f` - existential quantification (fresh unification variable binding a la minikanren)
- `Fp f` - fixpoint operator
- `Map t u` - pattern matching and replacement
- `Cstr p` - constraint assertion

The `Prog` type is parameterized by:
- `f`: The functor for data representation
- `n`: The type of variable names
- `p`: The type of propositions (constraints)

The library also provides a `dual` function that computes the dual of a relation:

- `dual Star = Star`
- `dual (Comp p q) = Comp (dual q) (dual p)` - reverses composition order
- `dual (Map t u) = Map u t` - swaps the matched terms

## Examples

### S-Expressions

The library includes an example implementation for S-expressions, which are:
- Atoms (string constants)
- Lists of S-expressions
- Variables

The constraint system for S-expressions is based on unification equations.

### Natural Number Addition

An example implementation of addition on natural numbers (using Peano encoding):
- `z` represents zero
- `s[n]` represents the successor of n

The `additionEx2` relation defines addition with the rules:
- 0 + b = b
- s(a) + b = s(a + b)

Running forward computes the sum:
```haskell
runProgramPair (s (s z), s (s (s z))) additionEx2
-- Result: [s (s (s (s (s z))))]  -- 2 + 3 = 5
```

Running the dual relation computes all pairs that sum to a given number:
```haskell
runProgram (s (s (s (s (s z))))) (dual additionEx2)
-- Results: [(0,5), (1,4), (2,3), (3,2), (4,1), (5,0)]
```

### Polytypic Hylomorphisms

The library includes a module for defining polytypic hylomorphisms, which allow the generation of recursive operators over polynomial functors:

- `X` - Variable type
- `C` - Constant type
- `Sum [Poly]` - Sum of polynomial functors
- `Prod [Poly]` - Product of polynomial functors

The main building blocks are:

1. `polyMap` - Generates a map operator for a polynomial functor
2. `hylo` - Creates a hylomorphism from a polynomial functor, coalgebra, and algebra

For example, addition can be defined as a hylomorphism over `Sum [C, X]`:

```haskell
-- Coalgebra for addition
addCoalg = Or
  (Ex $ \b -> Map (list [atom "z", var b]) (in1 (var b)))
  (Ex $ \a -> Ex $ \b -> Map (list [s (var a), var b]) (in2 (list [var a, var b])))

-- Algebra for addition
addAlg = Or
  (Ex $ \b -> Map (in1 (var b)) (var b))
  (Ex $ \a -> Map (in2 (var a)) (s (var a)))

-- Addition as a hylomorphism
additionEx3 = hylo (Sum [C, X]) addCoalg addAlg

-- Test: compute 2 + 3
runProgram (list [s (s z), s (s (s z))]) additionEx3
-- Result: [s (s (s (s (s z))))]  -- 5
```

## Usage

```haskell
import GenRMC

-- Define an append relation for lists
appendProg :: Eq n => Prog SExpF n
appendProg = Ex $ \out -> 
  Ex $ \xs ->
  Ex $ \ys ->
  Or
    (Comp 
      (Map (list [atom "nil", var ys, var ys]) (var out))
      Star)
    (Ex $ \x ->
     Ex $ \xs' ->
     Ex $ \zs ->
     Comp
       (Map (list [list [var x, var xs'], var ys, list [var x, var zs]]) (var out))
       (Comp (appendProg `substIn` [(out, list [var xs', var ys, var zs])]) Star))

-- Run a query
query = Map (list [list [atom "a", list [atom "b", atom "nil"]], 
                  list [atom "c", atom "nil"],
                  var 0]) 
            (var 0)

results = take 1 $ run (var 0) (Comp appendProg (Comp query Star))
-- Results: [list [atom "a", list [atom "b", list [atom "c", atom "nil"]]]]
```

## Installation

```
cabal install
```

## Running Tests

```
cabal test
```