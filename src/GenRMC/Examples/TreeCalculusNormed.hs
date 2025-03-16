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

-- | Tree calculus functor
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

-- Id search based on test cases
testSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
testSearch =
  Ex $ \dummy -> Ex $ \prog ->
    compAll [
      andAll [
        compAll [
          Map (var dummy) (f (var prog) l),
          treeCalcApp,
          Map l (var prog)
        ],
        compAll [
          Map (var dummy) (f (var prog) (b l)),
          treeCalcApp,
          Map (b l) (var prog)
        ],
        compAll [
          Map (var dummy) (f (var prog) (f l l)),
          treeCalcApp,
          Map (f l l) (var prog)
        ],
        compAll [
          Map (var dummy) (f (var prog) (b (b l))),
          treeCalcApp,
          Map (b (b l)) (var prog)
        ]
      ]
    ]


-- Id search based on test cases by computing backwards
backSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
backSearch =
  Ex $ \dummy -> Ex $ \prog ->
    compAll [
      andAll [
        compAll [
          Map (var dummy) l,
          dual treeCalcApp,
          Map (f (var prog) l) (var prog)
        ],
        compAll [
          Map (var dummy) (b l),
          dual treeCalcApp,
          Map (f (var prog) (b l)) (var prog)
        ],
        compAll [
          Map (var dummy) (f l l),
          dual treeCalcApp,
          Map (f (var prog) (f l l)) (var prog)
        ],
        compAll [
          Map (var dummy) (b (b l)),
          dual treeCalcApp,
          Map (f (var prog) (b (b l))) (var prog)
        ]
      ]
    ]

-- Search for a successor function
succSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
succSearch =
  Ex $ \dummy -> Ex $ \prog ->
    andAll [
      compAll [
        Map (var dummy) (f (var prog) l),
        treeCalcApp,
        Map (b l) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (b l)),
        treeCalcApp,
        Map (f l l) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (f l l)),
        treeCalcApp,
        Map (b (b l)) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (b (b l))),
        treeCalcApp,
        Map (f l (b l)) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (f l (b l))),
        treeCalcApp,
        Map (f (b l) l) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (f (b l) l)),
        treeCalcApp,
        Map (b (f l l)) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (f l (f l l))),
        treeCalcApp,
        Map (f (b l) (b l)) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (f (b l) (b l))),
        treeCalcApp,
        Map (f (f l l) l) (var prog)
      ],
      compAll [
        Map (var dummy) (f (var prog) (f (f l l) l)),
        treeCalcApp,
        Map (b (b (b l))) (var prog)
      ]
    ]


sCase :: (Ord n, Enum n) => n -> n -> TreeCalc n -> TreeCalc n -> TreeCalc n -> TreeCalc n -> Prog TreeCalcF n (TreeCalcProp n)
sCase dummy prog ft gt xt ot = 
  Ex $ \f' -> Ex $ \fg' -> 
  compAll [
    Map (var dummy) (f (var prog) ft),
    treeCalcApp,
    Map (var f') (f (var f') gt),
    treeCalcApp,
    Map (var fg') (f (var fg') xt),
    treeCalcApp,
    Map ot (var prog)
  ]

sCase2 :: (Ord n, Enum n) => n -> n -> TreeCalc n -> TreeCalc n -> TreeCalc n -> TreeCalc n -> Prog TreeCalcF n (TreeCalcProp n)
sCase2 dummy prog ft gt xt ot = 
  Ex $ \f' -> Ex $ \fg' -> 
  andAll [
    compAll [
      Map (var dummy) (f (var prog) ft),
      treeCalcApp,
      Map (var f') (f (var f') gt),
      treeCalcApp,
      Map (var fg') (f (var fg') xt),
      treeCalcApp,
      Map ot (var prog)
    ],
    compAll [
      Map (var dummy) ot,
      dual treeCalcApp,
      Map (f (var fg') xt) (var fg'),
      dual treeCalcApp,
      Map  (f (var f') gt) (var f'),
      dual treeCalcApp,
      Map (f (var prog) ft) (var prog)
    ]
  ]


-- C combinator search based on test cases
cSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
cSearch =
  Ex $ \dummy -> Ex $ \prog ->
    andAll [
      sCase dummy prog l l l (f l (f l l)),
      sCase dummy prog (b l) l l (f (b l) (f l l)),
      sCase dummy prog (f (f (f l l) l) l) (f l (f l (b (b l)))) (f (f l l) (b (f l l))) l,
      sCase dummy prog (b (f l l)) (f l (f (f l l) l)) (f l (f (b l) l)) (f (b (f l l)) (f (f l l) l))
    ]

-- S combinator search based on test cases
sSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
sSearch =
  Ex $ \dummy -> Ex $ \prog ->
    andAll [
      sCase dummy prog (f (b l) (f (b l) l)) (b l) (f l (f (f l l) l)) (f (f l l) l),
      sCase dummy prog (f (f (b l) (b l)) (b (b l))) (f (f l (f l l)) l) (f (f l (f l l)) (f l l)) l,
      sCase dummy prog (f (f (b l) l) l) (f (f l (b l)) (f (b l) l)) (f (b l) (f (f l l) l)) (f (b l) (f (b l) (f (f l l) l))),
      sCase dummy prog (b (b (b l))) (f l (f (b l) l)) (f l (b (b l))) (b (b l))
    ]


-- S combinator search based on test cases
dSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
dSearch =
  Ex $ \dummy -> Ex $ \prog ->
    andAll [
      sCase dummy prog (f (f l (b (b l))) l) (f (b l) (b (f l l))) (f (f l (f l l)) (f (b l) l)) (f l l),
      sCase dummy prog (b (b l)) (f (f (f l l) l) (f l l)) (f (b (f l l)) (b l)) (f l (f (b l) (f l (b l)))),
      sCase dummy prog (f (f l l) (b (b (b l)))) (f (b (f l l)) (b (b l))) (f (f l (f l l)) (b l)) (f l (f (b (b l)) (f (f l (f l l)) (b l))))
    ]

kCase :: (Ord n, Enum n) => n -> n -> TreeCalc n -> TreeCalc n -> Prog TreeCalcF n (TreeCalcProp n)
kCase dummy prog ft xt = 
  Ex $ \fg' -> 
  compAll [
    Map (var dummy) (f (var prog) ft),
    treeCalcApp,
    Map (var fg') (f (var fg') xt),
    treeCalcApp,
    Map ft (var prog)
  ]

-- K combinator search based on test cases
kSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
kSearch =
  Ex $ \dummy -> Ex $ \prog ->
    andAll [
      kCase dummy prog l l,
      kCase dummy prog (b l) l,
      kCase dummy prog (f l l) l,
      kCase dummy prog (b (b l)) l,
      kCase dummy prog (b (b l)) (b l),
      kCase dummy prog (f l l) (b l),
      kCase dummy prog (f l l) (f l l),
      kCase dummy prog (b (b l)) (f l l),
      kCase dummy prog (f (f l l) l) (b l),
      kCase dummy prog (f (f l l) l) (f l l)
    ]

uCase :: (Ord n, Enum n) => n -> n -> TreeCalc n -> TreeCalc n -> TreeCalc n -> Prog TreeCalcF n (TreeCalcProp n)
uCase dummy prog ft xt ot = 
  Ex $ \fg' -> 
  compAll [
    Map (var dummy) (f (var prog) ft),
    treeCalcApp,
    Map (var fg') (f (var fg') xt),
    treeCalcApp,
    Map ot (var prog)
  ]

uCase2 :: (Ord n, Enum n) => n -> n -> TreeCalc n -> TreeCalc n -> TreeCalc n -> Prog TreeCalcF n (TreeCalcProp n)
uCase2 dummy prog ft xt ot = 
  Ex $ \fg' ->
  andAll [
    compAll [
      Map (var dummy) (f (var prog) ft),
      treeCalcApp,
      Map (var fg') (f (var fg') xt),
      treeCalcApp,
      Map ot (var prog)
    ],
    compAll [
      Map (var dummy) (var prog),
      dual treeCalcApp,
      Map (f (var fg') xt) (var fg'),
      dual treeCalcApp,
      Map (f (var prog) ft) (var prog)
    ]
  ]

uSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
uSearch =
  Ex $ \dummy -> Ex $ \prog ->
    andAll [
      uCase2 dummy prog l l l,
      uCase2 dummy prog (b l) l l,
      uCase2 dummy prog (b l) (b l) (b l),
      uCase2 dummy prog (f l l) (b l) (f l (b l)),
      uCase2 dummy prog l (f l l) (f l l),
      uCase2 dummy prog (f (b l) l) (f l l) l
    ]