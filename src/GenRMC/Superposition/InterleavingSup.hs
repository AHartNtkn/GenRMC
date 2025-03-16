{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module GenRMC.Superposition.InterleavingSup 
  ( -- Export list
    InterleavingSup(..)
  , runInterleaving
  ) where

import Control.Monad.Free
import GenRMC.Types
import GenRMC.Core

-- | Rose tree implementation for Interleaving search
data RoseTree f n p
  = Leaf (Maybe Int) n (Free f n) [Prog f n p] p
  | Branch [RoseTree f n p]

-- | Helper function to combine trees for union operation
combineTrees :: [RoseTree f n p] -> [RoseTree f n p] -> [RoseTree f n p]
combineTrees [] ys = ys
combineTrees xs [] = xs
combineTrees xs ys = [Branch xs, Branch ys]

-- | Interleaving search implementation of Sup using a rose tree
-- The state is represented by a tree that stores (counter, n, Free f n, [Prog f n p], p) at its leaves
newtype InterleavingSup f n p = InterleavingSup { unInterleavingSup :: [RoseTree f n p] }

-- | Semigroup instance for InterleavingSup with proper branch handling
instance Semigroup (InterleavingSup f n p) where
  (<>) (InterleavingSup xs) (InterleavingSup ys) = InterleavingSup (combineTrees xs ys)

-- | Monoid instance for InterleavingSup
instance Monoid (InterleavingSup f n p) where
  mempty = InterleavingSup []

instance Sup f n p (InterleavingSup f n p) where
  isEmpty (InterleavingSup xs) = null xs
  
  singleton counter nextSym d ps cs = InterleavingSup [Leaf counter nextSym d ps cs]
  
  -- Interleaving search strategy is defined here
  fullStep _ (InterleavingSup []) = (Nothing, InterleavingSup [])
  fullStep stepFn (InterleavingSup trees) = walk trees
    where
      walk [] = (Nothing, InterleavingSup [])

      -- When we find a leaf with an empty program list, return it as a result and remove it from the tree
      walk (Leaf _ _ d [] p : rest) =
        (Just (d, p), InterleavingSup rest)
      
      -- When we find a leaf with non-empty program list, process it normally
      walk (Leaf counter n d ps p : rest) =
        let newSup = stepFn counter n d ps p
        in (Nothing, InterleavingSup $ rotateBranch (Branch (unInterleavingSup newSup) : rest))
      
      -- For a branch, process its leftmost element, then rotate
      walk (Branch [] : rest) = walk rest  -- Skip empty branches
      walk (Branch left : rest) =
        let (result, InterleavingSup processed) = walk left
            newLevel = case processed of
              [x] -> x : rest -- Flatten singleton branches
              newTrees -> Branch newTrees : rest
        in  (result, InterleavingSup $ rotateBranch newLevel)

-- | Handle rotation of branches
rotateBranch :: [RoseTree f n p] -> [RoseTree f n p]
rotateBranch [] = []
rotateBranch [x] = [x]
rotateBranch (x:xs) = xs ++ [x]

runInterleaving :: forall n f p. (Ord n, Enum n, Functor f, Prop f n p) => Maybe Int -> Free f n -> Prog f n p -> [(Free f n, p)]
runInterleaving = run (mempty :: InterleavingSup f n p) (toEnum 0)