{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}


module GenRMC.Superposition.DFSSup 
  ( -- Export list
    DFSSup(..)
  , runDFS
  ) where

import Control.Monad.Free
import GenRMC.Types
import GenRMC.Core

-- | Depth-first search implementation of Sup
-- Each state keeps track of its own next variable counter and step counter
newtype DFSSup f n p = DFSSup { unDFSSup :: [(Maybe Int, n, Free f n, [Prog f n p], p)] }

-- | Semigroup instance for DFSSup
instance Semigroup (DFSSup f n p) where
  (<>) (DFSSup xs) (DFSSup ys) = DFSSup (xs ++ ys)

-- | Monoid instance for DFSSup
instance Monoid (DFSSup f n p) where
  mempty = DFSSup []

instance Sup f n p (DFSSup f n p) where
  isEmpty (DFSSup xs) = null xs
  singleton counter nextSym d ps cs = DFSSup [(counter, nextSym, d, ps, cs)]
  
  -- Search strategy is defined here. In this case, we are doing a depth-first search
  fullStep _ (DFSSup []) = (Nothing, DFSSup [])
  fullStep _ (DFSSup ((_, _, datum, [], cs):rest)) = 
    (Just (datum, cs), DFSSup rest)
  fullStep stepFn (DFSSup ((counter, next, datum, progs, cs):rest)) =
    let newStates = stepFn counter next datum progs cs
    in fullStep stepFn (newStates `union` DFSSup rest)

runDFS :: forall n f p. (Ord n, Enum n, Functor f, Prop f n p) => Maybe Int -> Free f n -> Prog f n p -> [(Free f n, p)]
runDFS = run (mempty :: DFSSup f n p) (toEnum 0)