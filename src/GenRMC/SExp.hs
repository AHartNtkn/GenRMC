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
data SExpF x = Atom String | Cons x x
  deriving (Eq, Functor)

-- | Show instance for SExpF
instance Show x => Show (SExpF x) where
  show (Atom s) = s
  show (Cons h t) = "(" ++ show h ++ " " ++ show t ++ ")"

-- | Type alias for S-expressions - for actual computation
type SExp n = Free SExpF n

-- | Pretty printing helper for S-expressions
prettyPrintSExp :: Show n => SExp n -> String
prettyPrintSExp (Pure n) = show n
prettyPrintSExp (Free (Atom s)) = s
prettyPrintSExp (Free (Cons h t)) = prettyListSExp h t

-- | Helper for pretty-printing list-like S-expressions
prettyListSExp :: Show n => SExp n -> SExp n -> String
prettyListSExp h (Free (Atom "nil")) = "(" ++ prettyPrintSExp h ++ ")"
-- Special case for "s" operator
prettyListSExp (Free (Atom "s")) t = "(s " ++ prettyPrintSExp t ++ ")"
prettyListSExp h (Free (Cons h' t')) = 
  "(" ++ prettyPrintSExp h ++ " " ++ prettyPrintSExp (Free (Cons h' t')) ++ ")"
-- If tail is something other than nil or another cons, print as a regular list with elements
prettyListSExp h t = "(" ++ prettyPrintSExp h ++ " " ++ prettyPrintSExp t ++ ")"

-- | Helper functions to create S-expressions
atom :: String -> SExp n
atom s = Free (Atom s)

-- | Create a nil value (empty list)
nil :: SExp n
nil = atom "nil"

-- | Create a cons cell (pair)
cons :: SExp n -> SExp n -> SExp n
cons h t = Free (Cons h t)

-- | Convert a Haskell list to an S-expression list
list :: [SExp n] -> SExp n
list = foldr cons nil

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
    Right subst -> [(true, subst)]

-- | Orient equations into a substitution map
orientEquations :: Ord n => [Equation n] -> Either String (Map n (SExp n))
orientEquations = foldl addEquation (Right Map.empty)
  where
    addEquation (Left err) _ = Left err
    addEquation (Right subst) (Equation e1 e2) =
      let t1' = substData subst e1
          t2' = substData subst e2
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
        (Free (Cons h1 t1), Free (Cons h2 t2)) ->
          foldl addEquation (Right subst) [Equation h1 h2, Equation t1 t2]
        _ -> Left "Cannot unify different constructors"

-- | Check if a variable occurs in a term
occursCheck :: Ord n => n -> SExp n -> Bool
occursCheck v = go
  where
    go (Pure v') = v == v'
    go (Free (Atom _)) = False
    go (Free (Cons h t)) = go h || go t


-- | Example program: append relation
-- appendProg represents append(xs, ys, zs) where xs ++ ys = zs
appendProg :: Ord n => Prog SExpF n (SExpProp n)
appendProg = Fp $ \self -> 
  Or
    -- Base case: nil ++ ys = ys
    (Ex $ \ys ->
     Map (cons nil (var ys)) (var ys))
    
    -- Recursive case: (x:xs') ++ ys = x:(xs' ++ ys)
    (Ex $ \x ->
     Ex $ \xs' ->
     Ex $ \ys ->
     Comp
       -- Deconstruct the input: from (x:xs', ys) to (xs', ys)
       (Map (cons (cons (var x) (var xs')) (var ys)) (cons (var xs') (var ys)))
       (Comp
         -- Run recursive call on (xs', ys)
         self
         -- Construct the output: from result of (xs' ++ ys) to x:(xs' ++ ys) 
         (Ex $ \rest ->
           Map (var rest) (cons (var x) (var rest)))))

-- | Helper for substitution in a program
substIn :: (Ord n) => Prog SExpF n (SExpProp n) -> [(n, SExp n)] -> Prog SExpF n (SExpProp n)
substIn prog pairs = substProg (Map.fromList pairs) prog