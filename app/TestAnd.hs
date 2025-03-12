module Main where

import GenRMC
import GenRMC.Types
import GenRMC.SExp
import GenRMC.Superposition.DFSSup

-- | Display a list of S-expressions nicely
displayResults :: Show n => [SExp n] -> IO ()
displayResults results = do
  putStrLn $ "Found " ++ show (length results) ++ " results:"
  mapM_ (putStrLn . ("  " ++)) (map prettyPrintSExp results)

-- | Simple relation that succeeds if input equals "a"
isA :: Prog SExpF Int (SExpProp Int)
isA = Ex $ \x -> Map (var x) (atom "a")

-- | Simple relation that succeeds if input equals "b"
isB :: Prog SExpF Int (SExpProp Int)
isB = Ex $ \x -> Map (var x) (atom "b")

-- | Unify first component with a
unifyFirst :: Prog SExpF Int (SExpProp Int)
unifyFirst = Ex $ \x -> Ex $ \y ->
             Comp (Map (cons (var x) (var y)) (var x)) (Map (atom "a") (cons (var x) (var y)))

unifyFirst2 :: Prog SExpF Int (SExpProp Int)
unifyFirst2 = Ex $ \x -> Ex $ \y ->
             Comp (Map (cons (var x) (var y)) (var x)) (Map (atom "c") (cons (var x) (var y)))


-- | Unify second component with b
unifySecond :: Prog SExpF Int (SExpProp Int)
unifySecond = Ex $ \x -> Ex $ \y ->
              Comp (Map (cons (var x) (var y)) (var y)) (Map (atom "b") (cons (var x) (var y)))

unifySecond2 :: Prog SExpF Int (SExpProp Int)
unifySecond2 = Ex $ \x -> Ex $ \y ->
              Comp (Map (cons (var x) (var y)) (var y)) (Map (atom "d") (cons (var x) (var y)))


main :: IO ()
main = do
  putStrLn "=== Testing And Constructor ==="
  
  putStrLn "\n1. Testing And with two failing relations:"
  let test1 = andAll [isA, isB]
  displayResults (map fst $ runDFS (atom "c") test1 :: [SExp Int])
  
  putStrLn "\n2. Testing And with one success, one failure:"
  let test2 = andAll [isA, isB]
  displayResults (map fst $ runDFS (atom "a") test2 :: [SExp Int])
  
  putStrLn "\n3. Testing And with both relations succeeding:"
  let test3 = andAll [isA, isA]
  displayResults (map fst $ runDFS (atom "a") test3 :: [SExp Int])
  
  putStrLn "\n4. Testing dual of And:"
  let test4 = dual (andAll [isA, isB])
  displayResults (map fst $ runDFS (atom "a") test4 :: [SExp Int])

  putStrLn "\n5. Nontrivial unification across branches:"
  let test5 = andAll [unifyFirst, unifySecond]
  displayResults (map fst $ runDFS (cons (var 100) (var 101)) test5 :: [SExp Int])

  putStrLn "\n6. Nontrivial unification across branches with dual:"
  let test6 = dual (andAll [unifyFirst, unifySecond])
  displayResults (map fst $ runDFS (cons (var 100) (var 101)) test6 :: [SExp Int])

  putStrLn "\n7. Nontrivial unification across branches with disjunction:"
  let test7 = andAll [Or unifyFirst unifyFirst2, Or unifySecond unifySecond2]
  displayResults (map fst $ runDFS (cons (var 100) (var 101)) test7 :: [SExp Int])

  putStrLn "\nDone."
