{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Poly where


import GenRMC.Types
import GenRMC.SExp

-- | Data type for polynomial functors
data Poly
  = X         -- ^ Variable
  | C         -- ^ Constant
  | Sum [Poly] -- ^ Sum of polynomials
  | Prod [Poly] -- ^ Product of polynomials
  deriving (Show, Eq)

-- | Constructors for injecting into sum types
in1, in2 :: Ord n => SExp n -> SExp n
in1 = cons (atom "in1")
in2 = cons (atom "in2")

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
          (Ex $ \a -> Map (cons inj (var a)) (var a))
          (Comp
            (polyMap poly f)
            (Ex $ \a -> Map (var a) (cons inj (var a))))
        ) injs xs
  in foldr1 Or branches
polyMap (Prod ps) f =
  exN (2 * length ps) $ \vars -> 
    let n = length ps
        -- Split the variables into input and output vars
        aVars = take n vars              -- Input variables (a1$, a2$, etc.)
        aaVars = drop n vars             -- Output variables (aa1$, aa2$, etc.)
        
        -- Build a nested cons structure from a list of variables
        buildNestedCons :: [n] -> SExp n
        buildNestedCons [] = error "buildNestedCons: empty list"
        buildNestedCons [x] = var x
        buildNestedCons (x:xs) = cons (var x) (buildNestedCons xs)
        
        -- Initial step: map from cons structure of vars to first var
        initialStep = Map (buildNestedCons aVars) (var (head aVars))
        
        -- Middle steps: process each element and map to next var
        middleSteps = zipWith3 
            (\poly inVar outVar -> 
                Comp
                    (polyMap poly f)
                    (Map (var outVar) (var inVar))
            ) 
            (init ps)                  -- All but the last polynomial
            (tail aVars)               -- a2$, a3$, etc. (all but first input var)
            (init aaVars)              -- aa1$, aa2$, etc. (all but last output var)
        
        -- Final step: last polynomial and map to the structure of output vars
        finalStep = Comp
            (polyMap (last ps) f)
            (Map (var (last aaVars)) (buildNestedCons aaVars))
        
        -- Chain everything together with composition
        composedSteps = foldr Comp finalStep (initialStep : middleSteps)
    in composedSteps

-- | Create a hylomorphism from a polynomial functor and coalgebra/algebra
hylo :: Ord n => Poly -> Prog SExpF n (SExpProp n) -> Prog SExpF n (SExpProp n) -> Prog SExpF n (SExpProp n)
hylo poly coalg alg = Fp $ \self -> Comp coalg (Comp (polyMap poly self) alg)
