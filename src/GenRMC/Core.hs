{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Core where

import Control.Monad.Free
import Data.Foldable (fold)

import GenRMC.Types

-- | Core step function for execution
step :: (Ord n, Enum n, Functor f, Prop f n p, Sup f n p s) 
     => Free f n -> [Prog f n p] -> p -> GenSym n s
step datum [] cs = return $ singleton datum [] cs
step datum (p:ps) cs = case p of
  Star -> return $ singleton datum ps cs
  Comp p1 p2 -> return $ singleton datum (p1:p2:ps) cs
  Or p1 p2 -> return $ union 
                (singleton datum (p1:ps) cs)
                (singleton datum (p2:ps) cs)
  Ex f -> do
    x <- fresh
    return $ singleton datum (f x:ps) cs
  Fp f -> return $ singleton datum (f (Fp f):ps) cs
  Map t u -> return $ fold $ do
    prop <- unify datum t
    (datum', ps', cs') <- propagate u ps (andProp cs prop)
    return $ singleton datum' ps' cs'
  Cstr pr -> return $ fold $ do
    (datum', ps', cs') <- propagate datum ps (andProp cs pr)
    return $ singleton datum' ps' cs'

-- | Propagate constraints through a state
propagate :: (Ord n, Functor f, Prop f n p)
          => Free f n -> [Prog f n p] -> p -> [(Free f n, [Prog f n p], p)]
propagate datum progs prop = do
  (prop', mp) <- normalize prop
  return (substData mp datum, map (substProg mp) progs, prop')

-- | Execute a program and return a stream of results
run :: forall n f p s. (Ord n, Enum n, Functor f, Prop f n p, Sup f n p s) => s -> Free f n -> Prog f n p -> [(Free f n, p)]
run _ datum prog = 
  let initState = singleton datum [prog] true :: s
      go states =
        let (mOut, states') = fullStep step states
        in case mOut of
          Nothing -> []
          Just out -> out : go states'
  in go initState

