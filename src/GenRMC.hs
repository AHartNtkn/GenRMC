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
    -- * Natural number examples
  , z
  , s
  , additionEx2
  , runDFS
  , runBFS
  , runInterleaving
  , Poly(..)
  , polyMap
  , hylo
  , in1
  , in2
  , addCoalg
  , addAlg
  , additionEx3
  ) where

import GenRMC.Types
import GenRMC.Core
import GenRMC.SExp
import GenRMC.Superposition.DFSSup
import GenRMC.Superposition.BFSSup
import GenRMC.Superposition.InterleavingSup
import GenRMC.Examples
import GenRMC.Poly