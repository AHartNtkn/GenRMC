{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module GenRMC.Unify.FirstOrder where

import Control.Monad.Free
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Classes (Eq1)
import GenRMC.Types (Prop(..), substData)

-- | Generic equation for first-order terms
data Equation f n = Equation (Free f n) (Free f n)

-- | Predicate that can analyze an f layer
newtype Pred f = Pred (forall r. f r -> [[(Pred f, r)]])

-- | Generic set of equations and predicates
data UnifyProp f n = UnifyProp 
  { equations :: [Equation f n]
  , predicates :: [(Pred f, Free f n)]
  }

-- | Unifiable class for functors that support first-order unification
class (Functor f, Foldable f) => Unifiable f n | f -> where
  -- | Checks if two functors can be unified and returns equations for their components
  zipMatch :: f (Free f n) -> f (Free f n) -> [[Equation f n]]

-- | Semigroup instance for UnifyProp
instance Semigroup (UnifyProp f n) where
  (<>) (UnifyProp eqs1 preds1) (UnifyProp eqs2 preds2) = 
    UnifyProp (eqs1 ++ eqs2) (preds1 ++ preds2)

-- | Monoid instance for UnifyProp
instance Monoid (UnifyProp f n) where
  mempty = UnifyProp [] []

-- | Make UnifyProp an instance of Prop
instance (Ord n, Eq1 f, Unifiable f n) => Prop f n (UnifyProp f n) where
  unify t1 t2 = [UnifyProp [Equation t1 t2] []]

  normalize prop = 
    [(UnifyProp eqs preds', subst)
    | (eqs, subst, preds') <- normalizeEqs (equations prop) Map.empty (predicates prop)
    ]
    where
      normalizeEqs :: [Equation f n] -> Map n (Free f n) -> [(Pred f, Free f n)] -> [([Equation f n], Map n (Free f n), [(Pred f, Free f n)])]
      normalizeEqs [] subst preds = do
        newPreds <- normalizePreds [(prd, substData subst term) | (prd, term) <- preds]
        return ([], subst, newPreds)
      normalizeEqs (Equation t1 t2 : eqs) subst preds =
        let t1' = substData subst t1
            t2' = substData subst t2
        in case (t1', t2') of
          (Pure v1, Pure v2) -> 
            if v1 == v2
              then normalizeEqs eqs subst preds
              else normalizeEqs eqs (Map.insert v1 (Pure v2) subst) preds
          (Pure v, t) | not (occursCheck v t) -> 
            normalizeEqs eqs (Map.insert v t subst) preds
          (t, Pure v) | not (occursCheck v t) -> 
            normalizeEqs eqs (Map.insert v t subst) preds
          (Free f1, Free f2) ->
            [ (subEqs ++ restEqs, finalSubst, finalPreds)
            | subEqs <- zipMatch f1 f2
            , (restEqs, finalSubst, finalPreds) <- normalizeEqs (subEqs ++ eqs) subst preds
            ]
          _ -> normalizeEqs eqs subst preds

      normalizePreds :: [(Pred f, Free f n)] -> [[(Pred f, Free f n)]]
      normalizePreds [] = [[]]
      normalizePreds ((prd, term):rest) = 
        case term of
          Pure _ -> [(prd, term) : restPreds | restPreds <- normalizePreds rest]
          Free f -> case prd of
            Pred analyze -> 
              [ newPreds ++ restPreds
              | conj <- analyze f
              , newPreds <- normalizePreds conj
              , restPreds <- normalizePreds rest
              ]

  substProp subst (UnifyProp eqs preds) = 
    UnifyProp 
      (map (applySubst subst) eqs)
      (map (\(prd, term) -> (prd, substData subst term)) preds)

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