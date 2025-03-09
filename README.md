# GenRMC - Generic Relational Programming in Haskell

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

#### Direct Implementation (additionEx2)

```haskell
-- | Addition relation example
-- additionEx2 implements a relation where:
-- - add(a, b, c) means a + b = c
-- Base case: 0 + b = b
-- Recursion: s(a) + b = s(a + b)
additionEx2 :: (Ord n, Enum n) => Prog SExpF n (SExpProp n)
additionEx2 = Fp $ \self ->
  Or
    (Ex $ \b -> Map (list [z, var b]) (var b))  -- 0 + b = b
    (Comp
      (Ex $ \a -> Ex $ \b -> Map (list [s (var a), var b]) (list [var a, var b]))  -- s(a) + b = (a, b)
      (Comp
        self  -- recursive call with (a, b)
        (Ex $ \a -> Map (var a) (s (var a)))))  -- result a becomes s(a)
```

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
  (Ex $ \b -> Map (cons z (var b)) (in1 (var b)))
  (Ex $ \a -> Ex $ \b -> Map (cons (s (var a)) (var b)) (in2 (cons (var a) (var b))))

-- Algebra for addition
addAlg = Or
  (Ex $ \b -> Map (in1 (var b)) (var b))
  (Ex $ \a -> Map (in2 (var a)) (s (var a)))

-- Addition as a hylomorphism
additionEx3 = hylo (Sum [C, X]) addCoalg addAlg

-- Test: compute 2 + 3
runProgram (cons (s (s z)) (s (s (s z)))) additionEx3
-- Result: [s (s (s (s (s z))))]  -- 5
```

## Usage

```haskell
import GenRMC

-- Define an append relation for lists
-- appendProg represents append(xs, ys, zs) where xs ++ ys = zs
appendProg :: Ord n => Prog SExpF n (SExpProp n)
appendProg = Fp $ \self -> 
  Or
    -- Base case: nil ++ ys = ys
    (Ex $ \ys ->
     Map (cons nil (var ys)) (var ys))
    
    -- Recursive case: (x:xs') ++ ys = x:(xs' ++ ys)
    (Ex $ \x ->
     Ex $ \xs' ->
     Ex $ \ys ->
     Comp
       -- Deconstruct the input: from (x:xs', ys) to (xs', ys)
       (Map (cons (cons (var x) (var xs')) (var ys)) (cons (var xs') (var ys)))
       (Comp
         -- Run recursive call on (xs', ys)
         self
         -- Construct the output: from result of (xs' ++ ys) to x:(xs' ++ ys) 
         (Ex $ \rest ->
           Map (var rest) (cons (var x) (var rest))))))

-- Run a query to append [a, b] and [c]
xs = list [atom "a", atom "b"] :: SExp Int
ys = list [atom "c"] :: SExp Int
results = runProgramPair (xs, ys) appendProg
-- Result: [list [atom "a", atom "b", atom "c"]]

-- Finding all ways to split [a, b, c] into two lists
output = list [atom "a", atom "b", atom "c"] :: SExp Int
splits = runProgram output (dual appendProg)
-- Results: [(nil,[a,b,c]), ([a],[b,c]), ([a,b],[c]), ([a,b,c],nil)]
```

## Installation

```
stack build
```

## Running Test

Several example executables are available to demonstrate the library's capabilities:

```
# Run addition examples
stack exec test-addition

# Run append examples
stack exec test-append

# Run S-expression unification tests
stack exec run-tests
```
