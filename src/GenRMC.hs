-- | Generic Relational Model Checking
module GenRMC 
  ( -- * Core types
    Prog(..)
  , Prop(..)
  , Sup(..)
    -- * Core functions
  , step
  , run
  , dual
  , substProg
  , substData
  , DFSSup(..)
  , BFSSup(..)
  , InterleavingSup(..)
  , runDFS
  , runBFS
  , runInterleaving
    -- * S-expression example
  , SExpF(..)
  , SExp
  , SExpProp(..)
  , Equation(..)
  , atom
  , list
  , var
  , appendProg
  , prettyPrintSExp
    -- * Polytypic hylomorphism
  , polyMap
  , hylo
    -- * Natural number examples  , in1
  , in2
  , z
  , s
  , additionEx2
  , addCoalg
  , addAlg
  , additionEx3
  -- * Tree Calculus
  , treeCalculusEval
  ) where

import GenRMC.Types
import GenRMC.Core
import GenRMC.SExp
import GenRMC.Superposition.DFSSup
import GenRMC.Superposition.BFSSup
import GenRMC.Superposition.InterleavingSup
import GenRMC.Examples.Addition
import GenRMC.Examples.TreeCalculus
import GenRMC.Poly