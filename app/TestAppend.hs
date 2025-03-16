module Main where

import GenRMC
import GenRMC.SExp
import GenRMC.Types
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  putStrLn "=== Testing Append Function ==="
  
  -- Test appending [a, b] and [c]
  putStrLn "\nTest 1: [a, b] ++ [c] = ?"
  let xs = list [atom "a", atom "b"] :: SExp Int
      ys = list [atom "c"] :: SExp Int
      expectedResult = list [atom "a", atom "b", atom "c"] :: SExp Int
      results = map fst $ runDFS Nothing (cons xs ys) appendProg
  
  putStrLn $ "Results (" ++ show (length results) ++ "):"
  mapM_ (putStrLn . ("  " ++) . prettyPrintSExp) results
  
  -- Test finding two lists that append to [a, b, c]
  putStrLn "\nTest 2: What two lists append to [a, b, c]?"
  let output = list [atom "a", atom "b", atom "c"] :: SExp Int
      rResults = map fst $ runDFS Nothing output (dual appendProg)
  
  putStrLn $ "Results (" ++ show (length rResults) ++ "):"
  mapM_ (putStrLn . ("  " ++) . prettyPrintSExp) rResults
  
  putStrLn "\nDone."