{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module GenRMC.Unify.FirstOrder where

import Control.Monad.Free
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ()

import GenRMC.Types (substData)

-- | Unifiable class for functors that support first-order unification
class (Functor f, Foldable f, Ord n) => Unifiable f n | f -> where
  -- | Checks if two functors can be unified and returns equations for their components
  zipMatch :: f (Free f n) -> f (Free f n) -> Either String [(Free f n, Free f n)]

-- | Generic equation for first-order terms
data Equation f n = Equation (Free f n) (Free f n)

-- | Generic set of equations
newtype UnifyProp f n = UnifyProp [Equation f n]

-- | Semigroup instance for UnifyProp
instance Semigroup (UnifyProp f n) where
  (<>) (UnifyProp xs) (UnifyProp ys) = UnifyProp (xs ++ ys)

-- | Monoid instance for UnifyProp
instance Monoid (UnifyProp f n) where
  mempty = UnifyProp []

-- | Normalize a set of equations into a substitution map
normalizeEquations :: (Unifiable f n) 
                   => [Equation f n] 
                   -> [(UnifyProp f n, Map n (Free f n))]
normalizeEquations eqs =
  case orientEquations eqs of
    Left _ -> []  -- Inconsistent equations
    Right subst -> [(mempty, subst)]

-- | Orient equations into a substitution map
orientEquations :: (Unifiable f n) 
                => [Equation f n] 
                -> Either String (Map n (Free f n))
orientEquations = foldl addEquation (Right Map.empty)
  where
    addEquation (Left err) _ = Left err
    addEquation (Right subst) (Equation e1 e2) =
      let t1' = substData subst e1
          t2' = substData subst e2
      in case (t1', t2') of
        (Pure v1, Pure v2) -> 
          if v1 == v2
            then Right subst
            else Right (Map.insert v1 (Pure v2) subst)
        (Pure v1, t) -> 
          if occursCheck v1 t
            then Left "Occurs check failed"
            else Right (Map.insert v1 t subst)
        (t, Pure v2) -> 
          if occursCheck v2 t
            then Left "Occurs check failed"
            else Right (Map.insert v2 t subst)
        (Free f1, Free f2) ->
          case zipMatch f1 f2 of
            Left err -> Left err
            Right eqs -> foldl addEquation (Right subst) (map (uncurry Equation) eqs)

-- | Check if a variable occurs in a term
occursCheck :: (Unifiable f n) => n -> Free f n -> Bool
occursCheck v (Pure v') = v == v'
occursCheck v (Free f) = any (occursCheck v) f

-- | Apply a substitution to an equation
applySubst :: (Ord n, Functor f) 
           => Map n (Free f n) 
           -> Equation f n 
           -> Equation f n
applySubst subst (Equation t1 t2) = 
  Equation (substData subst t1) (substData subst t2)