{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Examples where

import GenRMC.Types
import GenRMC.Core
import GenRMC.SExp

-- | Natural Enumbers using S-expressions
-- z represents zero
-- s[n] represents the successor of n
z :: SExp n
z = atom "z"

s :: SExp n -> SExp n
s n = cons (atom "s") n

-- | Addition relation example
-- additionEx2 implements a relation where:
-- - add(a, b, c) means a + b = c
-- Base case: 0 + b = b
-- Recursion: s(a) + b = s(a + b)
additionEx2 :: (Ord n, Enum n) => Prog SExpF n (SExpProp n)
additionEx2 = Fp $ \self ->
  Or
    (Ex $ \b -> Map (list [z, var b]) (var b)) -- 0 + b = b
    (Comp
      (Ex $ \a -> Ex $ \b -> Map (list [s (var a), var b]) (list [var a, var b])) -- s(a) + b = (a, b)
      (Comp
        self -- recursive call with (a, b)
        (Ex $ \a -> Map (var a) (s (var a)))) -- result a becomes s(a)
     )

-- | Run the program with input and show all results
runProgram :: (Ord n, Enum n) => SExp n -> Prog SExpF n (SExpProp n) -> [SExp n]
runProgram input prog = map fst $ run (mempty :: ListSup SExpF n (SExpProp n)) input prog

-- | Run the program with input pair and show all results
runProgramPair :: (Ord n, Enum n) => (SExp n, SExp n) -> Prog SExpF n (SExpProp n) -> [SExp n]
runProgramPair (a, b) = runProgram (cons a b)

-- | Example usages
-- Addition: 2 + 3 = 5
addExample1 :: [SExp Int]
addExample1 = runProgramPair (s (s z), s (s (s z))) additionEx2
-- Should result in [s (s (s (s (s z))))]

-- | Inverse computation: What are all the pairs (a,b) such that a+b = 5?
addInverseExample :: [SExp Int]
addInverseExample = runProgram (s (s (s (s (s z))))) (dual additionEx2)
-- Should result in pairs like (0,5), (1,4), (2,3), (3,2), (4,1), (5,0)

