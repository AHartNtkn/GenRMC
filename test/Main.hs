module Main where

import Test.Hspec
import Control.Monad.Free
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)

import GenRMC
import GenRMC.Types
import GenRMC.SExp

main :: IO ()
main = hspec $ do
  describe "S-expression unification" $ do
    it "can unify simple expressions" $ do
      let x = 1 :: Int
          y = 2 :: Int
          e1 = atom "foo"
          e2 = atom "foo"
          prop = head $ unify e1 e2
          norm = normalize prop
      length norm `shouldBe` 1
      fst (head norm) `shouldBe` SExpProp []
      
    it "applies substitution to propositions" $ do
      let x = 1 :: Int
          y = 2 :: Int
          e1 = var x
          e2 = var y
          prop = head $ unify e1 e2
          subst = Map.fromList [(x, atom "test")]
          substituted = substProp subst prop
      case substituted of
        SExpProp [Equation left right] -> do
          left `shouldBe` atom "test"
          right `shouldBe` var y
        _ -> expectationFailure "Expected a single equation"

    it "can unify variables with atoms" $ do
      let x = 1 :: Int
          y = 2 :: Int
          e1 = var x
          e2 = atom "foo"
          prop = head $ unify e1 e2
          norm = normalize prop
      length norm `shouldBe` 1
      snd (head norm) `shouldBe` Map.singleton x e2

    it "can unify structured expressions" $ do
      let x = 1 :: Int
          y = 2 :: Int
          e1 = list [var x, atom "b"]
          e2 = list [atom "a", var y]
          prop = head $ unify e1 e2
          norm = normalize prop
      length norm `shouldBe` 1
      snd (head norm) `shouldBe` Map.fromList [(x, atom "a"), (y, atom "b")]

    it "fails when structures don't match" $ do
      let x = 1 :: Int
          y = 2 :: Int
          e1 = list [var x, atom "b", atom "c"]
          e2 = list [atom "a", var y]
          prop = head $ unify e1 e2
          norm = normalize prop
      norm `shouldBe` []

  describe "Append relation" $ do
    it "can compute simple appends" $ do
      let query = Map (list [list [atom "a", list [atom "b", nil]], 
                            list [atom "c", nil],
                            var 0]) 
                      (var 0)
          initial = singleton 0 (var 0) [] mempty :: ListSup SExpF Int (SExpProp Int)
          results = take 1 $ run initial 0 (var 0) (Comp appendProg (Comp query Star))
      length results `shouldBe` 1
      let expected = list [atom "a", list [atom "b", list [atom "c", nil]]]
      fst (head results) `shouldBe` expected
  
  describe "Dual function" $ do
    it "correctly dualizes programs" $ do
      -- Star is self-dual
      (dual Star :: Prog SExpF Int (SExpProp Int)) `shouldBe` Star
      
      -- Comp reverses the order
      let p1 = Star :: Prog SExpF Int (SExpProp Int)
          p2 = Map (atom "a") (atom "b")
          dualComp = dual (Comp p1 p2)
      dualComp `shouldBe` Comp (dual p2) (dual p1)
      
      -- Map swaps the terms
      let p3 = Map (atom "x") (atom "y") :: Prog SExpF Int (SExpProp Int)
          dualMap = dual p3
      dualMap `shouldBe` Map (atom "y") (atom "x")
      
      -- Applying dual twice should return the original program
      dual (dual p2) `shouldBe` p2
  
  describe "Addition relation" $ do
    it "computes 2 + 3 = 5" $ do
      let results = runProgramPair (s (s z), s (s (s z))) additionEx2
      length results `shouldBe` 1
      head results `shouldBe` s (s (s (s (s z))))  -- 5
    
    it "computes all pairs that sum to 5 using dual" $ do
      let results = runProgram (s (s (s (s (s z))))) (dual additionEx2)
          expected = [
              list [z, s (s (s (s (s z))))],                    -- (0,5)
              list [s z, s (s (s (s z)))],                      -- (1,4)
              list [s (s z), s (s (s z))],                      -- (2,3)
              list [s (s (s z)), s (s z)],                      -- (3,2)
              list [s (s (s (s z))), s z],                      -- (4,1)
              list [s (s (s (s (s z)))), z]                     -- (5,0)
            ]
      sort results `shouldBe` sort expected