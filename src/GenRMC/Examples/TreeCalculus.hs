{-# LANGUAGE ScopedTypeVariables #-}

module GenRMC.Examples.TreeCalculus where

import GenRMC.Types
import GenRMC.SExp

-- | Tree calculus evaluator
-- Implements reduction rules for the tree calculus
treeCalculusEval :: (Ord n, Enum n) => Prog SExpF n (SExpProp n)
treeCalculusEval = Fp $ \self ->
  orAll [
    -- eval * = *
    Map (atom "*") (atom "*"),
    
    -- eval (* y) = (* y)
    exN 2 $ \[y, y'] -> compAll [
      Map (cons (atom "*") (var y)) (var y),
      self,
      Map (var y') (cons (atom "*") (var y'))
    ],
    
    -- eval ((* y) z) = ((* y) z)
    exN 4 $ \[y, y', z, z'] -> compAll [
      Map (cons (cons (atom "*") (var y)) (var z)) (var y),
      self,
      Map (var y') (var z),
      self,
      Map (var z') (cons (cons (atom "*") (var y')) (var z'))
    ],
    
    -- eval (((* *) y) z) = eval y
    exN 2 $ \[y, z] -> compAll [
      Map (cons (cons (cons (atom "*") (atom "*")) (var y)) (var z)) (var y),
      self
    ],
    
    -- eval (((* (* x)) y) z) = eval ((x z) (y z))
    exN 6 $ \[x, x', y, y', z, z'] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (atom "*") (var x))) (var y)) (var z)) (var x),
      self,
      Map (var x') (var y),
      self,
      Map (var y') (var z),
      self,
      Map (var z') (cons (cons (var x') (var z')) (cons (var y') (var z'))),
      self
    ],
    
    -- eval (((* ((* w) x)) y) *) = eval w
    exN 3 $ \[x, y, w] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (cons (atom "*") (var w)) (var x))) (var y)) (atom "*")) (var w),
      self
    ],

    -- eval (((* ((* w) x)) y) (* u)) = eval (x u)
    exN 6 $ \[w, x, x', y, u, u'] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (cons (atom "*") (var w)) (var x))) (var y)) (cons (atom "*") (var u))) (var x),
      self,
      Map (var x') (var u),
      self,
      Map (var u') (cons (var x') (var u')),
      self
    ],
  
    -- eval (((* ((* w) x)) y) ((* u) v)) = eval ((y u) v)
    exN 8 $ \[w, x, y, y', u, u', v, v'] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (cons (atom "*") (var w)) (var x))) (var y)) (cons (cons (atom "*") (var u)) (var v))) (var y),
      self,
      Map (var y') (var u),
      self,
      Map (var u') (var v),
      self,
      Map (var v') (cons (cons (var y') (var u')) (var v')),
      self
    ]
  ]

-- | Parellel tree calculus evaluator
-- Implements reduction rules for the tree calculus while making some parts parallel
treeCalculusEvalPar :: (Ord n, Enum n) => Prog SExpF n (SExpProp n)
treeCalculusEvalPar = Fp $ \self ->
  orAll [
    -- eval * = *
    Map (atom "*") (atom "*"),
    
    -- eval (* y) = (* y)
    exN 2 $ \[y, y'] -> compAll [
      Map (cons (atom "*") (var y)) (var y),
      self,
      Map (var y') (cons (atom "*") (var y'))
    ],
    
    -- eval ((* y) z) = ((* y) z)
    exN 4 $ \[y, y', z, z'] -> compAll [
      Map (cons (cons (atom "*") (var y)) (var z)) (cons (var y) (var z)),
      andAll [
        compAll [Map (cons (var y) (var z)) (var y), self, Map (var y') (cons (var y') (var z'))],
        compAll [Map (cons (var y) (var z)) (var z), self, Map (var z') (cons (var y') (var z'))]
      ],
      Map (cons (var y') (var z')) (cons (cons (atom "*") (var y')) (var z'))
    ],
    
    -- eval (((* *) y) z) = eval y
    exN 2 $ \[y, z] -> compAll [
      Map (cons (cons (cons (atom "*") (atom "*")) (var y)) (var z)) (var y),
      self
    ],
    
    -- eval (((* (* x)) y) z) = eval ((x z) (y z))
    exN 6 $ \[x, x', y, y', z, z'] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (atom "*") (var x))) (var y)) (var z)) 
          (cons (var x) (cons (var y) (var z))),
      andAll [
        compAll [Map (cons (var x) (cons (var y) (var z))) (var x), self, Map (var x') (cons (var x') (cons (var y') (var z')))],
        compAll [Map (cons (var x) (cons (var y) (var z))) (var y), self, Map (var y') (cons (var x') (cons (var y') (var z)))],
        compAll [Map (cons (var x) (cons (var y) (var z))) (var z), self, Map (var z') (cons (var x') (cons (var y') (var z')))]
      ],
      Map (cons (var x') (cons (var y') (var z'))) (cons (cons (var x') (var z')) (cons (var y') (var z'))),
      self
    ],
    
    -- eval (((* ((* w) x)) y) *) = eval w
    exN 3 $ \[x, y, w] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (cons (atom "*") (var w)) (var x))) (var y)) (atom "*")) (var w),
      self
    ],

    -- eval (((* ((* w) x)) y) (* u)) = eval (x u)
    exN 6 $ \[w, x, x', y, u, u'] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (cons (atom "*") (var w)) (var x))) (var y)) (cons (atom "*") (var u)))
          (cons (var x) (var u)),
      andAll [
        compAll [Map (cons (var x) (var u)) (var x), self, Map (var x') (cons (var x') (var u'))],
        compAll [Map (cons (var x) (var u)) (var u), self, Map (var u') (cons (var x') (var u'))]
      ],
      self
    ],
  
    -- eval (((* ((* w) x)) y) ((* u) v)) = eval ((y u) v)
    exN 8 $ \[w, x, y, y', u, u', v, v'] -> compAll [
      Map (cons (cons (cons (atom "*") (cons (cons (atom "*") (var w)) (var x))) (var y)) (cons (cons (atom "*") (var u)) (var v)))
          (cons (var y) (cons (var u) (var v))),
      andAll [
        compAll [Map (cons (var y) (cons (var u) (var v))) (var y), self, Map (var y') (cons (var y') (cons (var u') (var v')))],
        compAll [Map (cons (var y) (cons (var u) (var v))) (var u), self, Map (var u') (cons (var y') (cons (var u') (var v')))],
        compAll [Map (cons (var y) (cons (var u) (var v))) (var v), self, Map (var v') (cons (var y') (cons (var u') (var v')))]
      ],
      self
    ]
  ]
