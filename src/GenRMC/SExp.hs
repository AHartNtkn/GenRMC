{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module GenRMC.SExp where

import Control.Monad.Free
import qualified Data.Map as Map

import GenRMC.Types
import GenRMC.Unify.FirstOrder

-- | S-expression functor
data SExpF x = Atom String | Cons x x
  deriving (Eq, Functor, Foldable)

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

-- | Type alias for generic equations with SExpF
type SExpEquation n = Equation SExpF n

-- | Set of equations for S-expressions
type SExpProp n = UnifyProp SExpF n

-- | Make SExpF an instance of Unifiable
instance Unifiable SExpF n where
  zipMatch (Atom s1) (Atom s2)
    | s1 == s2 = [[]]
    | otherwise = []
  zipMatch (Cons h1 t1) (Cons h2 t2) = 
    [[Equation h1 h2, Equation t1 t2]]
  zipMatch _ _ = []

-- | Make SExpProp an instance of Prop
instance Ord n => Prop SExpF n (SExpProp n) where  
  unify t1 t2 = [UnifyProp [Equation t1 t2]]
  
  normalize (UnifyProp eqs) =
    [(mempty, subst) | subst <- orientEquations eqs]
  
  substProp subst (UnifyProp eqs) = UnifyProp (map (applySubst subst) eqs)

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