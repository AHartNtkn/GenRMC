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
  putStrLn "\nSearching for identity function using test cases:"
  displayFullResults (take 1 $ runDFS Nothing l testSearch :: [(TreeCalc Int, TreeCalcProp Int)])
  putStrLn "\nSearching for identity function using test cases backwards:"
  displayFullResults (take 1 $ runInterleaving Nothing l backSearch :: [(TreeCalc Int, TreeCalcProp Int)])
  putStrLn "\nSearching for identity function using universal quantification:"
  displayFullResults (take 1 $ runDFS Nothing l idSearchU :: [(TreeCalc Int, TreeCalcProp Int)])
--  putStrLn "\nSearching for the successor function using test cases:"
--  displayFullResults (take 1 $ runInterleaving l succSearch :: [(TreeCalc Int, TreeCalcProp Int)])
--  putStrLn "\nSearching for S combinator:"
--  displayFullResults (take 1 $ runInterleaving Nothing l sSearch :: [(TreeCalc Int, TreeCalcProp Int)])
--  putStrLn "\nSearching for D combinator:"
--  displayFullResults (take 1 $ runInterleaving Nothing l dSearch :: [(TreeCalc Int, TreeCalcProp Int)])
--  putStrLn "\nSearching for C combinator:"
--  displayFullResults (take 1 $ runInterleaving Nothing l cSearch :: [(TreeCalc Int, TreeCalcProp Int)])
--  putStrLn "\nSearching for K combinator:"
--  displayFullResults (take 1 $ runInterleaving Nothing l kSearch :: [(TreeCalc Int, TreeCalcProp Int)])
--  putStrLn "\nSearching for U combinator:"
--  displayFullResults (take 1 $ runInterleaving Nothing l uSearch :: [(TreeCalc Int, TreeCalcProp Int)])
  putStrLn "\nTests completed." 