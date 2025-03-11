{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Examples.Addition 
  ( -- Export the addition examples
    additionEx2
  , additionEx3
  , addCoalg
  , addAlg
  , z
  , s
  ) where

import GenRMC.Types
import GenRMC.SExp
import GenRMC.Poly

-- | Natural numbers using S-expressions
-- z represents zero
-- s[n] represents the successor of n
z :: SExp n
z = atom "z"

s :: SExp n -> SExp n
s = cons (atom "s")

-- | Addition example using relation composition
-- This implements addition using natural numbers in SExp format
additionEx2 :: (Ord n, Enum n) => Prog SExpF n (SExpProp n)
additionEx2 = Fp $ \self -> 
  Or (Ex $ \x -> Map (cons z (var x)) (var x)) -- 0 + y = y
     (exN 3 $ \[x, y, z'] ->                   -- S(x) + y = S(x + y)
       compAll [
         Map (cons (s (var x)) (var y)) (cons (var x) (var y)),
         self,
         Map (var z') (s (var z'))
       ])

-- | Example: Addition as a hylomorphism
-- | Coalgebra for addition
addCoalg :: Ord n => Prog SExpF n (SExpProp n)
addCoalg = Or
  (Ex $ \b -> Map (cons z (var b)) (in1 (var b)))
  (Ex $ \a -> Ex $ \b -> Map (cons (s (var a)) (var b)) (in2 (cons (var a) (var b))))

-- | Algebra for addition
addAlg :: Ord n => Prog SExpF n (SExpProp n)
addAlg = Or
  (Ex $ \b -> Map (in1 (var b)) (var b))
  (Ex $ \a -> Map (in2 (var a)) (s (var a)))

-- | Addition as a hylomorphism over sum[C, X]
additionEx3 :: Ord n => Prog SExpF n (SExpProp n)
additionEx3 = hylo (Sum [C, X]) addCoalg addAlg
