module Main where

import GenRMC
import GenRMC.Types
import GenRMC.SExp
import GenRMC.Examples
import GenRMC.Superposition.InterleavingSup

-- | Display a list of S-expressions nicely
displayResults :: Show n => [SExp n] -> IO ()
displayResults results = do
  putStrLn $ "Found " ++ show (length results) ++ " results:"
  mapM_ (putStrLn . ("  " ++)) (map prettyPrintSExp results)

main :: IO ()
main = do
  putStrLn "=== GenRMC Examples ==="

  putStrLn "\n1. Immediate termination:"
  displayResults (map fst $ runInterleaving (atom "a") (Star :: Prog SExpF Int (SExpProp Int)) :: [SExp Int])


  putStrLn "\n1a. Computing 2 + 3:"
  displayResults (map fst $ runInterleaving (cons (s (s z)) (s (s (s z)))) additionEx2 :: [SExp Int])
  
  putStrLn "\n2a. What numbers add up to 5? (using dual relation):"
  displayResults (map fst $ runInterleaving (s (s (s (s (s z))))) (dual additionEx2) :: [SExp Int])
  
  putStrLn "\n3a. Addition using polytypic hylomorphism (2 + 3):"
  displayResults (map fst $ runInterleaving (cons (s (s z)) (s (s (s z)))) additionEx3 :: [SExp Int])

  putStrLn "\n4a. What numbers add up to 5 using polytypic hylomorphism?:"
  displayResults (map fst $ runInterleaving (s (s (s (s (s z))))) (dual additionEx3) :: [SExp Int])
  
  putStrLn "\nDone."