{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module GenRMC.SBExp where

import Control.Monad.Free
import qualified Data.Map as Map

import GenRMC.Types
import GenRMC.Unify.FirstOrder

-- | S-expression functor
data SBExpF r x = Var r | Atom String | Cons x x | Bind (r -> x)
  deriving (Eq, Functor, Foldable)

-- | Type alias for S-expressions - for actual computation
type SBExp r n = Free (SBExpF r) n

-- | Pretty printing helper for S-expressions
prettyPrintSBExp :: Show n => SBExp n -> String
prettyPrintSBExp (Pure n) = show n
prettyPrintSBExp (Free (Atom s)) = s
prettyPrintSBExp (Free (Cons h t)) = prettyListSBExp h t

-- | Helper for pretty-printing list-like S-expressions
prettyListSBExp :: Show n => SBExp n -> SBExp n -> String
prettyListSBExp h (Free (Atom "nil")) = "(" ++ prettyPrintSBExp h ++ ")"
-- Special case for "s" operator
prettyListSBExp (Free (Atom "s")) t = "(s " ++ prettyPrintSBExp t ++ ")"
prettyListSBExp h (Free (Cons h' t')) = 
  "(" ++ prettyPrintSBExp h ++ " " ++ prettyPrintSBExp (Free (Cons h' t')) ++ ")"
-- If tail is something other than nil or another cons, print as a regular list with elements
prettyListSBExp h t = "(" ++ prettyPrintSBExp h ++ " " ++ prettyPrintSBExp t ++ ")"

-- | Helper functions to create S-expressions
atom :: String -> SBExp n
atom s = Free (Atom s)

-- | Create a nil value (empty list)
nil :: SBExp n
nil = atom "nil"

-- | Create a cons cell (pair)
cons :: SBExp n -> SBExp n -> SBExp n
cons h t = Free (Cons h t)

-- | Convert a Haskell list to an S-expression list
list :: [SBExp n] -> SBExp n
list = foldr cons nil

var :: n -> SBExp n
var = Pure

-- | Type alias for generic equations with SBExpF
type SBExpEquation n = Equation SBExpF n

-- | Set of equations for S-expressions
type SBExpProp n = UnifyProp SBExpF n

-- | Make SBExpF an instance of Unifiable
instance Ord n => Unifiable SBExpF n where
  zipMatch (Atom s1) (Atom s2)
    | s1 == s2 = [[]]
    | otherwise = []
  zipMatch (Cons h1 t1) (Cons h2 t2) = 
    [[Equation h1 h2, Equation t1 t2]]
  zipMatch _ _ = []

-- | Make SBExpProp an instance of Prop
instance Ord n => Prop SBExpF n (SBExpProp n) where  
  unify t1 t2 = [UnifyProp [Equation t1 t2]]
  
  normalize (UnifyProp eqs) =
    [(mempty, subst) | subst <- orientEquations eqs]
  
  substProp subst (UnifyProp eqs) = UnifyProp (map (applySubst subst) eqs)

-- | Example program: append relation
-- appendProg represents append(xs, ys, zs) where xs ++ ys = zs
appendProg :: Ord n => Prog SBExpF n (SBExpProp n)
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
substIn :: (Ord n) => Prog SBExpF n (SBExpProp n) -> [(n, SBExp n)] -> Prog SBExpF n (SBExpProp n)
substIn prog pairs = substProg (Map.fromList pairs) prog