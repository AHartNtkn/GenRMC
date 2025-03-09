{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenRMC.Types where

import Control.Monad.Free
import Control.Monad.State
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

-- | Helper to create existential quantification over n variables
-- Returns a list of variables to the continuation
exN :: (Ord n) => Int -> ([n] -> Prog f n p) -> Prog f n p
exN n f 
  | n <= 0 = error "exN requires at least 1 variable"
  | otherwise = Ex $ \x -> go [x] (n-1)
  where
    go vars 0 = f vars
    go vars k | k > 0 = Ex $ \x -> go (x : vars) (k-1)
    go vars _ = f vars

-- | Compute the dual of a program
dual :: Prog f n p -> Prog f n p
dual Star = Star
dual (Comp p q) = Comp (dual q) (dual p)  -- Reverse order for composition
dual (Or p q) = Or (dual p) (dual q)      -- Or is symmetric
dual (Ex f) = Ex (dual . f)               -- Existential remains existential
dual (Fp f) = Fp (dual . f . dual)        -- Wrap function with duals
dual (Map t u) = Map u t                  -- Swap the terms
dual (Cstr p) = Cstr p                    -- Constraints remain unchanged

-- | Prop is a class for proposition types that can be used in our language
class (Monoid p) => Prop f n p | p -> f n where
  -- | Unify two terms
  unify :: Free f n -> Free f n -> [p]
  -- | Normalize a proposition to a set of simpler propositions with variable substitutions
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
  -- empty and mappend are inherited from Monoid
  singleton :: Free f n -> [Prog f n p] -> p -> s
  -- | Extract a state and the rest of the states
  extract :: s -> Maybe ((Free f n, [Prog f n p], p), s)
  -- | Execute a step and process results
  fullStep :: (Ord n, Enum n) 
           => (Free f n -> [Prog f n p] -> p -> GenSym n s)
           -> s
           -> (Maybe (Free f n, p), s)
  
  -- Default implementations using Monoid
  empty :: s
  empty = mempty
  
  union :: s -> s -> s
  union = mappend

-- | GenSym monad for generating fresh variable names
newtype GenSym n a = GenSym { runGenSym :: State n a }
  deriving (Functor, Applicative, Monad)

-- | Generate a fresh variable
fresh :: (Enum n) => GenSym n n
fresh = GenSym $ do
  n <- get
  put (succ n)
  return n

-- | Run the GenSym monad with an initial name
runGenSymWith :: n -> GenSym n a -> a
runGenSymWith seed (GenSym m) = evalState m seed

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

substData :: (Ord n, Functor f) => Map n (Free f n) -> Free f n -> Free f n
substData subst = go
  where
    go (Pure n) = fromMaybe (Pure n) (Map.lookup n subst)
    go (Free f) = Free (fmap go f)