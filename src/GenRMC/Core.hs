{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Core where

import Control.Monad.Free
import Data.Foldable (fold)

import GenRMC.Types

-- | Core step function for execution
step :: (Ord n, Enum n, Functor f, Prop f n p, Sup f n p s) 
     => n -> Free f n -> [Prog f n p] -> p -> s
step nsym datum [] cs = singleton nsym datum [] cs
step nsym datum (p:ps) cs = case p of
  Star -> singleton nsym datum ps cs
  Comp p1 p2 -> singleton nsym datum (p1:p2:ps) cs
  Or p1 p2 -> union 
                (singleton nsym datum (p1:ps) cs)
                (singleton nsym datum (p2:ps) cs)
  Ex f -> singleton (succ nsym) datum (f nsym:ps) cs
  Fp f -> singleton nsym datum (f (Fp f):ps) cs
  Map t u -> fold $ do
    prop <- unify datum t
    (datum', ps', cs') <- propagate u ps (andProp cs prop)
    return $ singleton nsym datum' ps' cs'
  Cstr pr -> fold $ do
    (datum', ps', cs') <- propagate datum ps (andProp cs pr)
    return $ singleton nsym datum' ps' cs'

-- | Propagate constraints through a state
propagate :: (Ord n, Functor f, Prop f n p)
          => Free f n -> [Prog f n p] -> p -> [(Free f n, [Prog f n p], p)]
propagate datum progs prop = do
  (prop', mp) <- normalize prop
  return (substData mp datum, map (substProg mp) progs, prop')

-- | Execute a program and return a stream of results
run :: forall n f p s. (Ord n, Enum n, Functor f, Prop f n p, Sup f n p s) => s -> n -> Free f n -> Prog f n p -> [(Free f n, p)]
run _ seed datum prog = 
  let initState = singleton seed datum [prog] true :: s
      go states =
        if isEmpty states
          then []
          else let (mOut, states') = fullStep step states
               in case mOut of
                  Nothing -> go states'
                  Just out -> out : go states'
  in go initState

