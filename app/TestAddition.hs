module Main where

import GenRMC
import GenRMC.Types
import GenRMC.SExp
import GenRMC.Examples.Addition
import GenRMC.Superposition.DFSSup

-- | Display a list of S-expressions nicely
displayResults :: Show n => [SExp n] -> IO ()
displayResults results = do
  putStrLn $ "Found " ++ show (length results) ++ " results:"
  mapM_ (putStrLn . ("  " ++)) (map prettyPrintSExp results)

main :: IO ()
main = do
  putStrLn "=== GenRMC Examples ==="
  
  putStrLn "\n1. Computing 2 + 3:"
  displayResults (map fst $ runDFS Nothing (cons (s (s z)) (s (s (s z)))) additionEx2 :: [SExp Int])
  
  putStrLn "\n2. What numbers add up to 5? (using dual relation):"
  displayResults (map fst $ runDFS Nothing (s (s (s (s (s z))))) (dual additionEx2) :: [SExp Int])
  
  putStrLn "\n3. Addition using polytypic hylomorphism (2 + 3):"
  displayResults (map fst $ runDFS Nothing (cons (s (s z)) (s (s (s z)))) additionEx3 :: [SExp Int])

  putStrLn "\n4. What numbers add up to 5 using polytypic hylomorphism?:"
  displayResults (map fst $ runDFS Nothing (s (s (s (s (s z))))) (dual additionEx3) :: [SExp Int])
  
  putStrLn "\nDone."