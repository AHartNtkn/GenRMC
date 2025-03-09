{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module GenRMC.SExp where

import Control.Monad.Free
import Data.Map (Map)
import qualified Data.Map as Map

import GenRMC.Types

-- | S-expression functor
data SExpF x = Atom String | List [x]
  deriving (Eq, Functor)

-- | Show instance for SExpF
instance Show x => Show (SExpF x) where
  show (Atom s) = s
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"

-- | Type alias for S-expressions - for actual computation
type SExp n = Free SExpF n

-- | Pretty printing helper for S-expressions
prettyPrintSExp :: Show n => SExp n -> String
prettyPrintSExp (Pure n) = show n
prettyPrintSExp (Free (Atom s)) = s
prettyPrintSExp (Free (List xs)) = "(" ++ unwords (map prettyPrintSExp xs) ++ ")"

-- | Helper functions to create S-expressions
atom :: String -> SExp n
atom s = Free (Atom s)

list :: [SExp n] -> SExp n
list xs = Free (List xs)

var :: n -> SExp n
var = Pure

-- | Equation type for S-expressions
data Equation n = Equation (SExp n) (SExp n)

-- | Show instance for Equation
instance Show n => Show (Equation n) where
  show (Equation l r) = prettyPrintSExp l ++ " = " ++ prettyPrintSExp r

-- | Set of equations
newtype SExpProp n = SExpProp [Equation n]

-- | Show instance for SExpProp
instance Show n => Show (SExpProp n) where
  show (SExpProp eqs) = unlines $ map show eqs

-- | Semigroup instance for SExpProp
instance Semigroup (SExpProp n) where
  (<>) (SExpProp xs) (SExpProp ys) = SExpProp (xs ++ ys)

-- | Monoid instance for SExpProp
instance Monoid (SExpProp n) where
  mempty = SExpProp []

-- Make SExpProp an instance of Prop
instance Ord n => Prop SExpF n (SExpProp n) where
  true = SExpProp []
  
  andProp (SExpProp eqs1) (SExpProp eqs2) = SExpProp (eqs1 ++ eqs2)
  
  unify t1 t2 = [SExpProp [Equation t1 t2]]
  
  normalize (SExpProp eqs) = normalizeEquations eqs
  
  substProp subst (SExpProp eqs) = SExpProp (map applySubst eqs)
    where
      applySubst (Equation t1 t2) = Equation (substData subst t1) (substData subst t2)

-- | Normalize a set of equations into a substitution map
normalizeEquations :: Ord n => [Equation n] -> [(SExpProp n, Map n (SExp n))]
normalizeEquations eqs =
  case orientEquations eqs of
    Left _ -> []  -- Inconsistent equations
    Right subst -> [(SExpProp [], subst)]

-- | Orient equations into a substitution map
orientEquations :: Ord n => [Equation n] -> Either String (Map n (SExp n))
orientEquations = foldl addEquation (Right Map.empty)
  where
    addEquation (Left err) _ = Left err
    addEquation (Right subst) (Equation t1 t2) =
      let t1' = substData subst t1
          t2' = substData subst t2
      in case (t1', t2') of
        (Pure v1, t) -> 
          if occursCheck v1 t
            then Left "Occurs check failed"
            else Right (Map.insert v1 t subst)
        (t, Pure v2) -> 
          if occursCheck v2 t
            then Left "Occurs check failed"
            else Right (Map.insert v2 t subst)
        (Free (Atom s1), Free (Atom s2)) ->
          if s1 == s2 
            then Right subst 
            else Left $ "Cannot unify atoms " ++ s1 ++ " and " ++ s2
        (Free (List xs), Free (List ys)) ->
          if length xs == length ys
            then foldl addEquation (Right subst) (zipWith Equation xs ys)
            else Left "Cannot unify lists of different lengths"
        _ -> Left "Cannot unify different constructors"

-- | Check if a variable occurs in a term
occursCheck :: Ord n => n -> SExp n -> Bool
occursCheck v = go
  where
    go (Pure v') = v == v'
    go (Free (Atom _)) = False
    go (Free (List xs)) = any go xs

-- | List-based implementation of Sup
-- Each state keeps track of its own fresh variable counter
newtype ListSup f n p = ListSup { unListSup :: [(n, (Free f n, [Prog f n p], p))] }

-- | Semigroup instance for ListSup
instance Semigroup (ListSup f n p) where
  (<>) (ListSup xs) (ListSup ys) = ListSup (xs ++ ys)

-- | Monoid instance for ListSup
instance Monoid (ListSup f n p) where
  mempty = ListSup []

instance (Ord n, Enum n) => Sup SExpF n (SExpProp n) (ListSup SExpF n (SExpProp n)) where
  singleton d ps cs = ListSup [(toEnum 0, (d, ps, cs))]
  
  extract (ListSup []) = Nothing
  extract (ListSup ((_, state):xs)) = Just (state, ListSup xs)
  
  -- Search strategy is defined here. In this case, we are just doing a depth-first search
  fullStep _ (ListSup []) = (Nothing, ListSup [])
  fullStep _ (ListSup ((_, (datum, [], cs)):rest)) = 
    (Just (datum, cs), ListSup rest)
  fullStep stepFn (ListSup ((next, (datum, progs, cs)):rest)) =
    let newStatesM = stepFn datum progs cs
        newStatesWithNext = runGenSymWith next newStatesM
        -- Ensure each state gets a unique variable counter
        withNextVars = ListSup [(succ next, state) | (_, state) <- unListSup newStatesWithNext]
    in fullStep stepFn (withNextVars `union` ListSup rest)

-- | Example program: append relation
appendProg :: Ord n => Prog SExpF n (SExpProp n)
appendProg = Ex $ \out -> 
  Ex $ \_ ->
  Ex $ \ys ->
  Or
    (Comp 
      (Map (list [atom "nil", var ys, var ys]) (var out))
      Star)
    (Ex $ \x ->
     Ex $ \xs' ->
     Ex $ \zs ->
     Comp
       (Map (list [list [var x, var xs'], var ys, list [var x, var zs]]) (var out))
       (Comp (appendProg `substIn` [(out, list [var xs', var ys, var zs])]) Star))

-- | Helper for substitution in a program
substIn :: (Ord n) => Prog SExpF n (SExpProp n) -> [(n, SExp n)] -> Prog SExpF n (SExpProp n)
substIn prog pairs = substProg (Map.fromList pairs) prog