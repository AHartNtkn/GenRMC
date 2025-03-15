{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import GenRMC
import GenRMC.Types
import GenRMC.Examples.TreeCalculusNormed
import GenRMC.Superposition.InterleavingSup
import GenRMC.Superposition.DFSSup
import GenRMC.Unify.FirstOrder
import Control.Monad.Free
import Data.Functor.Classes

-- | Display full results including constraints
displayFullResults :: Show n => [(TreeCalc n, TreeCalcProp n)] -> IO ()
displayFullResults results = do
  putStrLn $ "Found " ++ show (length results) ++ " results:"
  mapM_ showResult (zip [1..] results)
  where
    showResult (i, (term, prop)) = do
      putStrLn $ "\nResult " ++ show i ++ ":"
      putStrLn $ "  Term: " ++ prettyPrintTreeCalc term

main :: IO ()
main = do
  putStrLn "\nTesting Tree Calculus Search with Interleaving:"
  putStrLn "\nSearching for identity function (first result only):"
  -- Take only the first result but show full information including constraints
  displayFullResults (take 2 $ runDFS l testSearch3 :: [(TreeCalc Int, TreeCalcProp Int)])
  putStrLn "\nSearch completed." 