{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Types where

import Control.Monad.Free
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | Prog is the core syntax of our relational programming language
data Prog f n p
  = Star
  | Comp (Prog f n p) (Prog f n p)
  | Or (Prog f n p) (Prog f n p)
  | Ex (n -> Prog f n p)
  | Fp (Prog f n p -> Prog f n p)
  | Map (Free f n) (Free f n)
  | Cstr p
  | And (Free f n) (Free f n) [Prog f n p] [Prog f n p]

-- | Helper to create existential quantification over n variables
-- Returns a list of variables to the continuation
exN :: (Ord n) => Int -> ([n] -> Prog f n p) -> Prog f n p
exN n f 
  | n <= 0 = error "exN requires at least 1 variable"
  | otherwise = Ex $ \x -> go [x] (n-1)
  where
    go vars k | k > 0 = Ex $ \x -> go (x : vars) (k-1)
    go vars _ = f vars

-- | Compose a list of programs sequentially
-- Empty list becomes Star (identity for composition)
compAll :: [Prog f n p] -> Prog f n p
compAll [] = Star
compAll [p] = p
compAll (p:ps) = Comp p (compAll ps)

-- | Combine a list of programs with Or in a balanced tree structure
-- This ensures logarithmic nesting depth for better performance
orAll :: [Prog f n p] -> Prog f n p
orAll [] = error "Cannot take the disjunction of an empty list"
orAll [p] = p
orAll ps = 
  let (left, right) = splitAt (length ps `div` 2) ps
  in Or (orAll left) (orAll right)

-- | Create a tensor from a list of programs in a balanced binary tree structure
-- This ensures logarithmic nesting depth for better performance
andAll :: [Prog f n p] -> Prog f n p
andAll [] = error "Cannot tensor an empty list"
andAll [p] = p
andAll ps = Ex $ \x -> Comp (Map (Pure x) (Pure x)) 
                            (buildTensorTree x ps)
  where
  -- | Helper function to build a balanced binary tree of tensors
  buildTensorTree :: n -> [Prog f n p] -> Prog f n p
  buildTensorTree _ [q] = q
  buildTensorTree x qs =
    let (left, right) = splitAt (length qs `div` 2) qs
        leftTree = buildTensorTree x left
        rightTree = buildTensorTree x right
    in And (Pure x) (Pure x) [leftTree] [rightTree]

-- | Compute the dual of a program
dual :: Prog f n p -> Prog f n p
dual Star = Star
dual (Comp p q) = Comp (dual q) (dual p)  -- Reverse order for composition
dual (Or p q) = Or (dual p) (dual q)      -- Or is symmetric
dual (Ex f) = Ex (dual . f)               -- Existential remains existential
dual (Fp f) = Fp (dual . f . dual)        -- Wrap function with duals
dual (Map t u) = Map u t                  -- Swap the terms
dual (Cstr p) = Cstr p                    -- Constraints remain unchanged
dual (And t u p q) = And t u (map dual $ reverse p) (map dual $ reverse q)

-- | Prop is a class for propositions over our terms
class (Monoid p) => Prop f n p | p -> f n where
  -- | Unify two terms
  unify :: Free f n -> Free f n -> [p]
  -- | Normalize a proposition to a set of simpler propositions with variable substitutions
  -- | The substitution map should arise out of variables being eliminated.
  normalize :: p -> [(p, Map n (Free f n))]
  -- | Apply a substitution to a proposition
  substProp :: Map n (Free f n) -> p -> p

  -- Default implementations using Monoid
  true :: p
  true = mempty
  
  andProp :: p -> p -> p
  andProp = mappend

-- | Sup represents a set of states in our execution
class (Monoid s) => Sup f n p s | s -> f n p where
  -- | Checks if the state is empty
  isEmpty :: s -> Bool

  singleton :: n -> Free f n -> [Prog f n p] -> p -> s

  -- | Execute a step and process results
  fullStep :: (Ord n, Enum n) 
           => (n -> Free f n -> [Prog f n p] -> p -> s)
           -> s
           -> (Maybe (Free f n, p), s)
  
  -- Default implementations using Monoid
  empty :: s
  empty = mempty
  
  union :: s -> s -> s
  union = mappend

-- Utilities for substitution
substProg :: (Ord n, Functor f, Prop f n p) => Map n (Free f n) -> Prog f n p -> Prog f n p
substProg subst = go
  where
    go Star = Star
    go (Comp p q) = Comp (go p) (go q)
    go (Or p q) = Or (go p) (go q)
    go (Ex f) = Ex (go . f)
    go (Fp f) = Fp (go . f)
    go (Map t u) = Map (substData subst t) (substData subst u)
    go (Cstr p) = Cstr (substProp subst p)
    go (And t u p q) = And (substData subst t) (substData subst u) (map go p) (map go q)

substData :: (Ord n, Functor f) => Map n (Free f n) -> Free f n -> Free f n
substData subst = go
  where
    go (Pure n) = fromMaybe (Pure n) (Map.lookup n subst)
    go (Free f) = Free (fmap go f)