{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE LambdaCase #-}

module GenRMC.Examples.TreeCalculusNormed where

import Control.Monad.Free
import GenRMC.Types
import GenRMC.Unify.FirstOrder
import Data.Functor.Classes (Eq1, liftEq)

-- | Tree calculus functor
data TreeCalcF x = C Int | L | B x | F x x
  deriving (Eq, Functor, Foldable, Show)

instance Eq1 TreeCalcF where
  liftEq eq (C n) (C n') = n == n'
  liftEq eq L L = True
  liftEq eq (B x) (B y) = eq x y
  liftEq eq (F x y) (F x' y') = eq x x' && eq y y'
  liftEq _ _ _ = False

type TreeCalc = Free TreeCalcF

-- | Set of equations for S-expressions
type TreeCalcProp n = UnifyProp TreeCalcF n

-- | Make TreeCalcF an instance of Unifiable
instance Unifiable TreeCalcF n where
  zipMatch (C n) (C n') = [[] | n == n']
  zipMatch L L = [[]]
  zipMatch (B x) (B y) = [[Equation x y]]
  zipMatch (F x y) (F x' y') = [[Equation x x', Equation y y']]
  zipMatch _ _ = []

c :: Int -> TreeCalc n
c n = Free (C n)

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
prettyPrintTreeCalc (Free (C n)) = "C_" ++ show n
prettyPrintTreeCalc (Free L) = "L"
prettyPrintTreeCalc (Free (B x)) = "B[" ++ prettyPrintTreeCalc x ++ "]"
prettyPrintTreeCalc (Free (F x y)) = "F[" ++ prettyPrintTreeCalc x ++ ", " ++ prettyPrintTreeCalc y ++ "]"

maxDepth :: Int -> Pred TreeCalcF
maxDepth i | i > 0 = Pred $ \case
  L -> [[]]
  B x -> [[(maxDepth (i - 1), x)]]
  F x y -> [[(maxDepth (i - 1), x), (maxDepth (i - 1), y)]]
  C _ -> []
maxDepth _ = Pred $ const []

-- | Tree calculus evaluator
-- Implements reduction rules for the tree calculus
treeCalcApp :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
treeCalcApp = Fp $ \self ->
  orAll [
    -- app[L, z_] := B[z]
    Ex $ \z -> 
      Map (f l (var z))
          (b (var z)),
    
    -- app[B[y_], z_] := F[y, z]
    Ex $ \y -> Ex $ \z -> 
      Map (f (b (var y)) (var z)) 
          (f (var y) (var z)),

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

-- Predicate to test if a universal variable is present
noUniversal :: Pred TreeCalcF
noUniversal = Pred $ \case
  L -> [[]]
  B x -> [[(noUniversal, x)]]
  F x y -> [[(noUniversal, x), (noUniversal, y)]]
  C _ -> []

-- Identity search using universal quantification
-- Found: F[B[B[L]], L]
idSearchU :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
idSearchU =
  Ex $ \dummy -> Ex $ \prog ->
    compAll [
      Cstr (UnifyProp [] [(noUniversal, var prog)]),
      Map (var dummy) (f (var prog) (c 0)),
      treeCalcApp,
      Map (c 0) (var prog)
    ]

-- Search of an f such that f x y = F[x, y]
-- Found: L
stage1Search :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
stage1Search =
  Ex $ \dummy -> Ex $ \prog ->
    Ex $ \c0' ->
    compAll [
      Cstr (UnifyProp [] [(noUniversal, var prog)]),
      Map (var dummy) (f (var prog) (c 0)),
      treeCalcApp,
      Map (var c0') (f (var c0') (c 1)),
      treeCalcApp,
      Map (f (c 0) (c 1)) (var prog)
    ]

-- Search of an f such that f x y z = F[F[x, y], z]
-- Found: F[B[F[L, B[B[F[L, L]]]]], L]
stageSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
stageSearch =
  Ex $ \dummy -> Ex $ \prog ->
    exN 2 $ \[c0', c1'] ->
    compAll [
      Cstr (UnifyProp [] [(noUniversal, var prog)]),
      Map (var dummy) (f (var prog) (c 0)),
      treeCalcApp,
      Map (var c0') (f (var c0') (c 1)),
      treeCalcApp,
      Map (var c1') (f (var c1') (c 2)),
      treeCalcApp,
      Map (f (f (c 0) (c 1)) (c 2)) (var prog)
    ]

-- Search for f such that f F[F[x, y], z] = F[x, F[y, z]]
assocSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
assocSearch =
  Ex $ \dummy -> Ex $ \prog ->
    compAll [
      Cstr (UnifyProp [] [(noUniversal, var prog)]),
      Map (var dummy) (f (var prog) (f (f (c 0) (c 1)) (c 2))),
      treeCalcApp,
      Map (f (c 0) (f (c 1) (c 2))) (var prog)
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
      sCase dummy prog l l l (f l (b l)),
      sCase dummy prog (f (b l) (b l)) (f l (b l)) l (f l l),
      sCase dummy prog (b (b (b l))) (f l (f l l)) (f (b l) (b l)) (f l l),
      sCase dummy prog (f l l) (f (b l) l) (f (b l) l) (b (f (f (b l) l) (b (f (b l) l)))),
      sCase dummy prog l (f l (b l)) (f l (f l l)) (f (f l (f l l)) (b l))
    ]


-- D combinator search based on test cases
dSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
dSearch =
  Ex $ \dummy -> Ex $ \prog ->
    andAll [
      sCase dummy prog l l l (f l (b l)),
      sCase dummy prog (f l (b l)) (f (b l) (b l)) l (f l l),
      sCase dummy prog (f l (f l l)) (b (b (b l))) (f (b l) (b l)) (f l l),
      sCase dummy prog (f (b l) l) (f l l) (f (b l) l) (b (f (f (b l) l) (b (f (b l) l)))),
      sCase dummy prog (f l (b l)) l (f l (f l l)) (f (f l (f l l)) (b l))
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

-- K combinator search
-- Found: B[L]
kSearch :: (Ord n, Enum n) => Prog TreeCalcF n (TreeCalcProp n)
kSearch =
  Ex $ \dummy -> Ex $ \prog ->
    compAll [
      Cstr (UnifyProp [] [(noUniversal, var prog)]),
      kCase dummy prog (c 0) (c 1)
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