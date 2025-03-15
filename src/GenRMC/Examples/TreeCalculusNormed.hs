{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module GenRMC.Examples.TreeCalculusNormed where

import Control.Monad.Free
import GenRMC.Types
import GenRMC.Unify.FirstOrder
import Data.Functor.Classes (Eq1, liftEq)

-- | S-expression functor
data TreeCalcF x = L | B x | F x x
  deriving (Eq, Functor, Foldable, Show)

instance Eq1 TreeCalcF where
  liftEq eq L L = True
  liftEq eq (B x) (B y) = eq x y
  liftEq eq (F x y) (F x' y') = eq x x' && eq y y'
  liftEq _ _ _ = False

type TreeCalc = Free TreeCalcF

-- | Set of equations for S-expressions
type TreeCalcProp n = UnifyProp TreeCalcF n

-- | Make TreeCalcF an instance of Unifiable
instance Unifiable TreeCalcF n where
  zipMatch L L = [[]]
  zipMatch (B x) (B y) = [[Equation x y]]
  zipMatch (F x y) (F x' y') = [[Equation x x', Equation y y']]
  zipMatch _ _ = []

l :: TreeCalc n
l = Free L

b :: TreeCalc n -> TreeCalc n
b x = Free (B x)

f :: TreeCalc n -> TreeCalc n -> TreeCalc n
f x y = Free (F x y)

var :: n -> TreeCalc n
var = Pure

-- | Type alias for generic equations with TreeCalcF
type TreeCalcEquation n = Equation TreeCalcF n


prettyPrintTreeCalc :: Show n => TreeCalc n -> String
prettyPrintTreeCalc (Pure n) = show n
prettyPrintTreeCalc (Free L) = "L"
prettyPrintTreeCalc (Free (B x)) = "B[" ++ prettyPrintTreeCalc x ++ "]"
prettyPrintTreeCalc (Free (F x y)) = "F[" ++ prettyPrintTreeCalc x ++ ", " ++ prettyPrintTreeCalc y ++ "]"

-- | Tree calculus evaluator
-- Implements reduction rules for the tree calculus
treeCalcApp :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
treeCalcApp = Fp $ \self ->
  orAll [
    -- app[L, z_] := F[L, z]
    Ex $ \z -> 
      Map (f l (var z))
          (f l (var z)),
    
    -- app[B[y_], z_] := F[B[y], z]
    Ex $ \y -> Ex $ \z -> 
      Map (f (b (var y)) (var z)) 
          (f (b (var y)) (var z)),

    -- app[F[L, y_], z_] := y
    Ex $ \y -> Ex $ \z -> 
      Map (f (f l (var y)) (var z))
          (var y),

    -- app[F[F[w_, x_], y_], L] := w
    exN 3 $ \[w, x, y] ->
      Map (f (f (f (var w) (var x)) (var y)) l)
          (var w),

    -- app[F[B[x_], y_], z_] := app[app[x, z], app[y, z]]
    exN 5 $ \[x, y, z, ol, or] ->
      compAll [
        andAll [
          compAll [
            Map (f (f (b (var x)) (var y)) (var z))
                (f (var x) (var z)),
            self,
            Map (var ol) (f (var ol) (var or))
          ],
          compAll [
            Map (f (f (b (var x)) (var y)) (var z))
                (f (var y) (var z)),
            self,
            Map (var or) (f (var ol) (var or))
          ]
        ],
        self
      ],

    -- app[F[F[w_, x_], y_], B[u_]] := app[x, u]
    exN 4 $ \[w, x, y, u] ->
      compAll [
        Map (f (f (f (var w) (var x)) (var y)) (b (var u)))
            (f (var x) (var u)),
        self
      ],

    -- app[F[F[w_, x_], y_], F[u_, v_]] := app[app[y, u], v]
    exN 6 $ \[w, x, y, u, v, o] ->
      compAll [
        Map (f (f (f (var w) (var x)) (var y)) (f (var u) (var v)))
            (f (var y) (var u)),
        self,
        Map (var o) (f (var o) (var v)),
        self
      ]
  ]
