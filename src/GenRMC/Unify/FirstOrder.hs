{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module GenRMC.Unify.FirstOrder where

import Control.Monad.Free
import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ()

import GenRMC.Types (substData)

-- | Generic equation for first-order terms
data Equation f n = Equation (Free f n) (Free f n)

-- | Unifiable class for functors that support first-order unification
class (Functor f, Foldable f) => Unifiable f n | f -> where
  -- | Checks if two functors can be unified and returns equations for their components
  zipMatch :: f (Free f n) -> f (Free f n) -> [[Equation f n]]

-- | Generic set of equations
newtype UnifyProp f n = UnifyProp [Equation f n]

-- | Semigroup instance for UnifyProp
instance Semigroup (UnifyProp f n) where
  (<>) (UnifyProp xs) (UnifyProp ys) = UnifyProp (xs ++ ys)

-- | Monoid instance for UnifyProp
instance Monoid (UnifyProp f n) where
  mempty = UnifyProp []

-- | Orient equations into possible substitution maps
orientEquations :: (Ord n, Unifiable f n) 
                => [Equation f n] 
                -> [Map n (Free f n)]
orientEquations = foldM addEquation Map.empty
  where
    addEquation :: (Ord n, Unifiable f n) => Map n (Free f n) -> Equation f n -> [Map n (Free f n)]
    addEquation subst (Equation t1 t2) =
      let t1' = substData subst t1
          t2' = substData subst t2
      in case (t1', t2') of
        (Pure v1, Pure v2) -> 
          if v1 == v2
            then [subst]
            else [Map.insert v1 (Pure v2) subst]
        (Pure v1, t) -> [Map.insert v1 t subst | not (occursCheck v1 t)]
        (t, Pure v2) -> [Map.insert v2 t subst | not (occursCheck v2 t)]
        (Free f1, Free f2) -> concatMap (foldM addEquation subst) $ zipMatch f1 f2

-- | Check if a variable occurs in a term
occursCheck :: (Ord n,Unifiable f n) => n -> Free f n -> Bool
occursCheck v (Pure v') = v == v'
occursCheck v (Free f) = any (occursCheck v) f

-- | Apply a substitution to an equation
applySubst :: (Ord n, Functor f) 
           => Map n (Free f n) 
           -> Equation f n 
           -> Equation f n
applySubst subst (Equation t1 t2) = 
  Equation (substData subst t1) (substData subst t2)