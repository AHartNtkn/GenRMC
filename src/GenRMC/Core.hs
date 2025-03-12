{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Core where

import Control.Monad.Free
import Data.Foldable (fold)

import GenRMC.Types
import GenRMC.Unify.FirstOrder (Equation(Equation))

-- | Core step function for execution
step :: (Ord n, Enum n, Functor f, Prop f n p) 
     => n -> Free f n -> [Prog f n p] -> p -> [(n, Free f n, [Prog f n p], p)]
step nsym datum [] cs = [(nsym, datum, [], cs)]
step nsym datum (p:ps) cs = case p of
  Star -> [(nsym, datum, ps, cs)]
  Comp p1 p2 -> [(nsym, datum, p1:p2:ps, cs)]
  Or p1 p2 -> [(nsym, datum, p1:ps, cs), (nsym, datum, p2:ps, cs)]
  Ex f -> [(succ nsym, datum, f nsym:ps, cs)]
  Fp f -> [(nsym, datum, f (Fp f):ps, cs)]
  Map t u -> do
    prop <- unify datum t
    return (nsym, u, ps, andProp cs prop)
  Cstr pr -> do
    return (nsym, datum, ps, andProp cs pr)
  And (Just x) t1 t2 p1 p2 -> do
    prop <- unify (Pure x) datum
    return (nsym, t1, And Nothing t1 t2 p1 p2:ps, andProp cs prop)
  And Nothing t1 t2 [] [] -> do
    prop <- unify t1 t2
    return (nsym, t1, ps, andProp cs prop)
  And Nothing t1 t2 p1 p2 -> do
    (nsym', t1', p1', cs') <- step nsym t1 p1 cs
    (nsym'', t2', p2', cs'') <- step nsym' t2 p2 cs'
    return (nsym'', datum, And Nothing t1' t2' p1' p2':ps, cs'')

-- | Propagate constraints through a state
propagate :: (Ord n, Functor f, Prop f n p)
          => Free f n -> [Prog f n p] -> p -> [(Free f n, [Prog f n p], p)]
propagate datum progs prop = do
  (prop', mp) <- normalize prop
  return (substData mp datum, map (substProg mp) progs, prop')

-- | Step then propogate
stepProp :: (Ord n, Enum n, Functor f, Prop f n p, Sup f n p s) 
     => n -> Free f n -> [Prog f n p] -> p -> s
stepProp nsym datum ps prop = 
  let stepResults = step nsym datum ps prop
      propResults = concatMap (\(n, d, ps', pr) -> 
                              map (\(d', ps'', pr') -> (n, d', ps'', pr')) 
                                  (propagate d ps' pr)) 
                             stepResults
      singles = map (\(n, d, ps', pr) -> singleton n d ps' pr) propResults
  in fold singles

-- | Execute a program and return a stream of results
run :: forall n f p s. (Ord n, Enum n, Functor f, Prop f n p, Sup f n p s) => s -> n -> Free f n -> Prog f n p -> [(Free f n, p)]
run _ seed datum prog = 
  let initState = singleton seed datum [prog] true :: s
      go states =
        if isEmpty states
          then []
          else let (mOut, states') = fullStep stepProp states
               in case mOut of
                  Nothing -> go states'
                  Just out -> out : go states'
  in go initState

