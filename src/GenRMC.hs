-- | Generic Relational Model Checking
module GenRMC 
  ( -- * Core types
    Prog(..)
  , Prop(..)
  , Sup(..)
  , GenSym
  , fresh
  , runGenSymWith
    -- * Core functions
  , step
  , run
  , dual
  , substProg
  , substData
    -- * S-expression example
  , SExpF(..)
  , SExp
  , SExpProp(..)
  , Equation(..)
  , ListSup(..)
  , atom
  , list
  , var
  , appendProg
  , prettyPrintSExp
    -- * Natural number examples
  , z
  , s
  , additionEx2
  , runProgram
  , runProgramPair
    -- * Polytypic programming
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
import GenRMC.Examples
import GenRMC.Poly