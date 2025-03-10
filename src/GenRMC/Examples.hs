{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Examples where

import GenRMC.Types
import GenRMC.SExp

-- | Natural Enumbers using S-expressions
-- z represents zero
-- s[n] represents the successor of n
z :: SExp n
z = atom "z"

s :: SExp n -> SExp n
s = cons (atom "s")

-- | Addition relation example
-- additionEx2 implements a relation where:
-- - add(a, b, c) means a + b = c
-- Base case: 0 + b = b
-- Recursion: s(a) + b = s(a + b)
additionEx2 :: (Ord n, Enum n) => Prog SExpF n (SExpProp n)
additionEx2 = Fp $ \self ->
  Or
    (Ex $ \b -> Map (cons z (var b)) (var b)) -- 0 + b = b
    (Comp
      (Ex $ \a -> Ex $ \b -> Map (cons (s (var a)) (var b)) (cons (var a) (var b))) -- s(a) + b = (a, b)
      (Comp
        self -- recursive call with (a, b)
        (Ex $ \a -> Map (var a) (s (var a)))) -- result a becomes s(a)
     )

