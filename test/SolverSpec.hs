module SolverSpec where

import Test.Hspec
import SATSolver.CNF
import SATSolver.Solver

toClause :: [Int] -> Clause
toClause = map readLiteral

toCNF :: [[Int]] -> CNF
toCNF = map toClause

testCNF :: CNF
testCNF = toCNF [[1, 2], [-1, 3], [-2, -3], [-1, 2, -3]]


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findUnitClauses" $ do
    it "should find proper unit clauses" $ do
      findUnitClauses testCNF                  `shouldBe` []
      findUnitClauses (toClause [1]:testCNF) `shouldBe` [PosLit 1]
      findUnitClauses ([]:testCNF)             `shouldBe` []

  describe "removeLitFromClause" $ do
    it "should properly remove literals" $ do
      removeLitFromClause (PosLit 1) (toClause [1, 2])  `shouldBe` Nothing
      removeLitFromClause (PosLit 1) (toClause [-1, 2]) `shouldBe` Just [PosLit 2]
      removeLitFromClause (PosLit 3) (toClause [-1, 2]) `shouldBe` Just [NegLit 1, PosLit 2]
      removeLitFromClause (PosLit 1) (toClause [-1])      `shouldBe` Just []
      removeLitFromClause (NegLit 1) (toClause [1])       `shouldBe` Just []
      removeLitFromClause (PosLit 1) (toClause [1])       `shouldBe` Nothing

  -- describe "simplifyCNF" $ do
  --   it "should correctly simplify with unit clauses" $ do
  --     snd (simplifyCNF (toClause [3]:testCNF))  `shouldBe` [[]]
  --     simplifyCNF (toClause [-3]:testCNF)  `shouldBe` (toClause [2, -1, -3], [])
  --
  describe "findPureLiterals" $ do
    it "should correctly find pure literals" $ do
      findPureLiterals (toCNF [[3], [3], [-2]])                 `shouldBe` toClause [3, -2]
      findPureLiterals (toCNF [[-3], [3], [-2]])                `shouldBe` toClause [-2]
      findPureLiterals (toCNF [[], [3], [-2]])                    `shouldBe` toClause [3, -2]
      findPureLiterals (toCNF [[], [3], [-2]])                    `shouldBe` toClause [3, -2]
      findPureLiterals (toCNF [[3, 2], [-2, -3]])             `shouldBe` toClause []
      findPureLiterals (toCNF [[1], [3, 2, 1], [-2, -3]]) `shouldBe` toClause [1]

  describe "MAXO" $ do
    it "Should return optimal literal by MAXO" $ do
      maxo (toCNF [[3], [3], [-2]]) `shouldBe` PosLit 3
      maxo (toCNF [[3], [-2], [-2, -3], [2]]) `shouldBe` NegLit 2
      maxo (toCNF [[1]]) `shouldBe` PosLit 1
      maxo (toCNF [[1, 1, 1, 2], [2]]) `shouldBe` PosLit 1 -- this is perhaps not ok...

  describe "UP" $ do
    it "Should return optimal literal by UP" $ do
      up (PosLit 3) (toCNF [[3], [3], [-2]]) `shouldBe` 1
      up (PosLit 2) (toCNF [[1], [-2], [-2, -3], [2]]) `shouldBe` 2
      up (NegLit 1) (toCNF [[1], [-2, 1], [-2, -3], [2, 3]]) `shouldBe` 1
      -- maxo (toCNF [[1]]) `shouldBe` PosLit 1
      -- maxo (toCNF [[1, 1, 1, 2], [2 3] [""]]) `shouldBe` PosLit 1
