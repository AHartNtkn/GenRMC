module Main where

import GenRMC
import GenRMC.Types
import GenRMC.Examples.TreeCalculusNormed
import GenRMC.Superposition.DFSSup

displayResults :: Show n => [TreeCalc n] -> IO ()
displayResults results = do
  putStrLn $ "Found " ++ show (length results) ++ " results:"
  mapM_ (putStrLn . ("  " ++)) (map prettyPrintTreeCalc results)

main :: IO ()
main = do
  putStrLn "\nTesting Tree Calculus Normalization:"
  putStrLn "\n1. Should result in \"B[L]\":"
  displayResults (map fst $ runDFS (f
        (f (f l (f (b (b l)) l)) (f (f (b l) l) (f l l)))
        (f (f (b (b (f l l))) (b l)) (f (f l (f l l)) (b (f l l))))
        ) treeCalcApp :: [TreeCalc Int])
  putStrLn "\n2. Should result in \"F[L, F[F[L, B[L]], F[L, B[L]]]]\":"
  displayResults (map fst $ runDFS (f
        (f (b (f l l)) (f (b l) l))
        (f (f l (b l)) (f l (b l)))
        ) treeCalcApp :: [TreeCalc Int])
  putStrLn "\n3. Should result in \"L\":"
  displayResults (map fst $ runDFS (f
        (f (f (f (b l) (b (f l l))) (b (f l l))) (f (b (b (b l))) (f l (b l))))
        (f (f (f l l) (f (f l l) (b l))) (f (f l l) (f (b (f l l)) (b l))))
        ) treeCalcApp :: [TreeCalc Int])
  putStrLn "\n4. Should result in \"F[L, F[L, F[B[B[L]], F[B[L], B[L]]]]]\":"
  displayResults (map fst $ runDFS (f
        (f (b (f l l)) l)
        (f (b (b l)) (f (b l) (b l)))
        ) treeCalcApp :: [TreeCalc Int])
  putStrLn "\n5. Should result in \"F[L, L]\":"
  displayResults (map fst $ runDFS (f
        (f (f (f (f (f (f (f (b l) (b l)) (f (b l) (f (b l) l))) (f (f (f l (b l)) l) (f l (f (b l) l)))) (f (f (b l) (f (b (b l)) l)) (f (b (b (b l))) (f (b l) (f l l))))) (f (f (f (f l l) (f (b (b l)) l)) (f (f l (f l (b l))) (f (f l (b l)) l))) (f (f (b (b (f l l))) (b l)) (f (f (b l) (f (b l) l)) (f (b l) l))))) (f (f (f (f (f (f l (b l)) (b l)) (f l l)) (f l (f (f (b l) l) (b l)))) (f (f (f l l) (f (b (f l l)) l)) (f (f l (f l (b l))) (f (b (b l)) l)))) (f (f (f (f l (b (b l))) (f (f l (b l)) l)) (f (f (b l) l) (f (f l (b l)) l))) (f (f (f (b l) (b l)) (b (f (b l) (b l)))) (f (b l) (f l (f l l))))))) (f (f (f (f (f (f (f l (b l)) l) (f l (b l))) (f (b (f l l)) (f l (f l l)))) (f (f (f (f l (b l)) (b l)) (f (f l (b l)) (f l l))) (f (f l (b l)) (f (b l) (f l (b l)))))) (f (f (f (b (b l)) (b (b l))) (f (f (f l l) (f l l)) (b (b (b l))))) (f (f (f (f l (b l)) l) (f l l)) (f (b (f l l)) (f l (b l)))))) (f (f (f (f (f (f l l) l) (b (f l l))) (f (f (f l l) (b l)) (b (b l)))) (f (f (f l (f (b l) l)) l) (f (b l) (f (b l) (f (b l) (b l)))))) (f (f (f (f (f l l) (f l l)) (f (f l (b l)) (b l))) (b (f (b l) l))) (f (f (f (b (b l)) l) (f (b l) (f (b l) l))) (f (b (b l)) (f (b l) (f l (b l)))))))))
        (f (f (f (f (f (f (f (b l) (b (b l))) (b (f l l))) (f (f l (b (b l))) (b (f l (b l))))) (f (f (b (b l)) (f l (b l))) (f (f (f l (f l l)) (b l)) (f (b l) (b l))))) (f (f (f (f (b (b l)) (b l)) (b (f l l))) (f l (f (b l) (f l l)))) (f (f (f (b (b l)) l) (f (f l l) (f (b l) l))) (f (b (b l)) (b (b l)))))) (f (f (f (f (b (f l l)) l) (f (f l l) (f (f l l) (b (b l))))) (f (f (f (b (b l)) (f l (b l))) (f l (f l l))) (f (f (b l) l) (f l (b (b l)))))) (f (f (f l (f (b (b l)) (f (b l) l))) (f (f (b l) (f l l)) (f (f l l) (b l)))) (f (f (f (b l) (b l)) (b (b l))) (f l (f l (f l (f l l)))))))) (f (f (f (f (f (f (b (b l)) l) (f (b (b l)) (b l))) (f (f l (f (b l) l)) (b (f (b l) l)))) (f (f (f (f l l) l) (b l)) (f (f l (f l l)) (b (b l))))) (f (f (f (f l (f l (b l))) (b l)) (f (f l l) (f (b l) (f (b l) l)))) (f (f (f (f (b l) l) (b l)) (f l l)) (b (f (b l) (f l l)))))) (f (f (f (f (f l (f l (b l))) (f (f l l) l)) (f (f (f (b l) l) l) (f (b l) l))) (f (f (f (f l l) l) (f (f l l) (f l (b l)))) (f (b l) (f (b l) (b l))))) (f (f (f (f (f l l) (f l (b l))) (f l (b l))) (f (b (f l (b l))) (f (b l) (b l)))) (f (f (b (f (b l) (b l))) l) (f (f (b l) (f (b l) l)) (f (f l l) l)))))))
        ) treeCalcApp :: [TreeCalc Int])

  putStrLn "\nAll tests completed."