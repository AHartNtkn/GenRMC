{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module GenRMC.Superposition.BFSSup 
  ( -- Export list
    BFSSup(..)
  , runBFS
  ) where

import Control.Monad.Free
import GenRMC.Types
import GenRMC.Core

-- | Depth-first search implementation of Sup
-- Each state keeps track of its own next variable counter
newtype BFSSup f n p = BFSSup { unBFSSup :: [(n, Free f n, [Prog f n p], p)] }

-- | Semigroup instance for BFSSup
instance Semigroup (BFSSup f n p) where
  (<>) (BFSSup xs) (BFSSup ys) = BFSSup (xs ++ ys)

-- | Monoid instance for BFSSup
instance Monoid (BFSSup f n p) where
  mempty = BFSSup []

instance Sup f n p (BFSSup f n p) where
  isEmpty (BFSSup xs) = null xs

  singleton nextSym d ps cs = BFSSup [(nextSym, d, ps, cs)]
  
  -- Search strategy is defined here. In this case, we are doing a breadth-first search
  fullStep _ (BFSSup []) = (Nothing, BFSSup [])
  fullStep _ (BFSSup ((_, datum, [], cs):rest)) = 
    (Just (datum, cs), BFSSup rest)
  fullStep stepFn (BFSSup ((next, datum, progs, cs):rest)) =
    let newStates = stepFn next datum progs cs
    in fullStep stepFn (BFSSup rest `union` newStates)

runBFS :: forall n f p. (Ord n, Enum n, Functor f, Prop f n p) => Free f n -> Prog f n p -> [(Free f n, p)]
runBFS = run (mempty :: BFSSup f n p) (toEnum 0)