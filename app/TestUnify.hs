module Main where

import Control.Monad.Free
import Data.Map (Map)
import qualified Data.Map as Map

import GenRMC
import GenRMC.Types
import GenRMC.SExp
import GenRMC.Examples

-- | Display a list of S-expressions nicely
displayResults :: Show n => [SExp n] -> IO ()
displayResults results = do
  putStrLn $ "Found " ++ show (length results) ++ " results:"
  mapM_ (putStrLn . ("  " ++)) (map prettyPrintSExp results)

-- | Test S-expression unification
testUnify :: IO ()
testUnify = do
  let x = 1 :: Int
      y = 2 :: Int
      e1 = atom "foo" :: SExp Int
      e2 = atom "foo" :: SExp Int
      prop = head $ unify e1 e2 :: SExpProp Int
      norm = normalize prop :: [(SExpProp Int, Map Int (SExp Int))]
  
  putStrLn "Test 1: Unifying identical atoms"
  putStrLn $ "  e1 = " ++ prettyPrintSExp e1
  putStrLn $ "  e2 = " ++ prettyPrintSExp e2
  putStrLn $ "  Number of solutions: " ++ show (length norm)
  let SExpProp equations = fst (head norm)
  putStrLn $ "  Empty equations: " ++ show (null equations)
  putStrLn ""
  
  let e3 = var x :: SExp Int
      e4 = atom "foo" :: SExp Int
      prop2 = head $ unify e3 e4 :: SExpProp Int
      norm2 = normalize prop2 :: [(SExpProp Int, Map Int (SExp Int))]
  
  putStrLn "Test 2: Unifying variable with atom"
  putStrLn $ "  e3 = " ++ prettyPrintSExp e3
  putStrLn $ "  e4 = " ++ prettyPrintSExp e4
  putStrLn $ "  Number of solutions: " ++ show (length norm2)
  putStrLn $ "  Variable: " ++ show x
  putStrLn $ "  Maps to: " ++ prettyPrintSExp (Map.findWithDefault e3 x (snd (head norm2)))
  putStrLn ""
  
  let e5 = list [var x, atom "b"] :: SExp Int
      e6 = list [atom "a", var y] :: SExp Int
      prop3 = head $ unify e5 e6 :: SExpProp Int
      norm3 = normalize prop3 :: [(SExpProp Int, Map Int (SExp Int))]
  
  putStrLn "Test 3: Unifying structured expressions"
  putStrLn $ "  e5 = " ++ prettyPrintSExp e5
  putStrLn $ "  e6 = " ++ prettyPrintSExp e6
  putStrLn $ "  Number of solutions: " ++ show (length norm3)
  putStrLn $ "  Variable " ++ show x ++ " maps to: " ++ prettyPrintSExp (Map.findWithDefault e5 x (snd (head norm3)))
  putStrLn $ "  Variable " ++ show y ++ " maps to: " ++ prettyPrintSExp (Map.findWithDefault e6 y (snd (head norm3)))
  putStrLn ""
  
  let e7 = list [var x, atom "b", atom "c"] :: SExp Int
      e8 = list [atom "a", var y] :: SExp Int
      prop4 = head $ unify e7 e8 :: SExpProp Int
      norm4 = normalize prop4 :: [(SExpProp Int, Map Int (SExp Int))]
  
  putStrLn "Test 4: Unifying mismatched structures"
  putStrLn $ "  e7 = " ++ prettyPrintSExp e7
  putStrLn $ "  e8 = " ++ prettyPrintSExp e8
  putStrLn $ "  Number of solutions: " ++ show (length norm4)
  putStrLn ""

main :: IO ()
main = do
  putStrLn "=== Running S-Expression Unification Tests ==="
  putStrLn ""
  
  testUnify
  
  putStrLn "Done."