{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Poly where

import GenRMC.Types
import GenRMC.SExp
import GenRMC.Examples (z, s, runProgram)

-- | Data type for polynomial functors
data Poly
  = X         -- ^ Variable
  | C         -- ^ Constant
  | Sum [Poly] -- ^ Sum of polynomials
  | Prod [Poly] -- ^ Product of polynomials
  deriving (Show, Eq)

-- | Constructors for injecting into sum types
in1, in2 :: Ord n => SExp n -> SExp n
in1 x = list [atom "in1", x]
in2 x = list [atom "in2", x]

-- | Generate a polytypic map based on a polynomial functor
polyMap :: Ord n => Poly -> Prog SExpF n (SExpProp n) -> Prog SExpF n (SExpProp n)
polyMap X f = f
polyMap C _ = Star
polyMap (Sum xs) f =
  let numInjs = length xs
      -- Generate injection function names: in1, in2, ..., inN
      injs = [atom ("in" ++ show i) | i <- [1..numInjs]]
      -- Create a branch for each injection
      branches = zipWith (\inj poly -> 
        Comp
          (Ex $ \a -> Map (list [inj, var a]) (var a))
          (Comp
            (polyMap poly f)
            (Ex $ \a -> Map (var a) (list [inj, var a])))
        ) injs xs
  in foldr1 Or branches
polyMap (Prod xs) f =
  exN (2 * length xs) $ \vars -> 
    let n = length xs
        -- Split the variables into input and output vars
        aVars = take n vars               -- Input variables (a1$, a2$, etc.)
        aaVars = drop n vars              -- Output variables (aa1$, aa2$, etc.)
        
        -- Initial step: map from tuple of variables to first var
        initialStep = Map (list (map var aVars)) (var (head aVars))
        
        -- Middle steps: process each element and map to next var
        middleSteps = zipWith3 
            (\poly inVar outVar -> 
                Comp
                    (polyMap poly f)
                    (Map (var outVar) (var inVar))
            ) 
            (init xs)            -- All but the last polynomial
            (tail aVars)         -- a2$, a3$, etc. (all but first input var)
            (init aaVars)        -- aa1$, aa2$, etc. (all but last output var)
        
        -- Final step: last polynomial and map to the tuple of all output vars
        finalStep = Comp
            (polyMap (last xs) f)
            (Map (var (last aaVars)) (list (map var aaVars)))
        
        -- Chain everything together with composition
        composedSteps = foldr Comp finalStep (initialStep : middleSteps)
    in composedSteps

-- | Create a hylomorphism from a polynomial functor and coalgebra/algebra
hylo :: Ord n => Poly -> Prog SExpF n (SExpProp n) -> Prog SExpF n (SExpProp n) -> Prog SExpF n (SExpProp n)
hylo poly coalg alg = Fp $ \self -> Comp coalg (Comp (polyMap poly self) alg)

-- | Example: Addition as a hylomorphism
-- | Coalgebra for addition
addCoalg :: Ord n => Prog SExpF n (SExpProp n)
addCoalg = Or
  (Ex $ \b -> Map (list [atom "z", var b]) (in1 (var b)))
  (Ex $ \a -> Ex $ \b -> Map (list [s (var a), var b]) (in2 (list [var a, var b])))

-- | Algebra for addition
addAlg :: Ord n => Prog SExpF n (SExpProp n)
addAlg = Or
  (Ex $ \b -> Map (in1 (var b)) (var b))
  (Ex $ \a -> Map (in2 (var a)) (s (var a)))

-- | Addition as a hylomorphism over sum[C, X]
additionEx3 :: Ord n => Prog SExpF n (SExpProp n)
additionEx3 = hylo (Sum [C, X]) addCoalg addAlg

-- | Test the addition example with specific inputs
testAddition :: (Enum n, Ord n) => [SExp n]
testAddition = runProgram (list [s (s z), s (s (s z))]) additionEx3
-- Should compute 2 + 3 = 5