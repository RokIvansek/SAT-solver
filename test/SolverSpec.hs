module SolverSpec where

import Test.Hspec
import SATSolver.CNF
import SATSolver.Solver

toClause :: [String] -> Clause
toClause = map readLiteral

toCNF :: [[String]] -> CNF
toCNF = map toClause

testCNF :: CNF
testCNF = toCNF [["p", "q"], ["-p", "r"], ["-q", "-r"], ["-p", "q", "-r"]]


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findUnitClauses" $ do
    it "should find proper unit clauses" $ do
      findUnitClauses testCNF                  `shouldBe` []
      findUnitClauses (toClause ["p"]:testCNF) `shouldBe` [PosLit "p"]
      findUnitClauses ([]:testCNF)             `shouldBe` []

  describe "removeLiteral" $ do
    it "should properly remove literals" $ do
      removeLiteral (PosLit "p") (toClause ["p", "q"])  `shouldBe` Nothing
      removeLiteral (PosLit "p") (toClause ["-p", "q"]) `shouldBe` Just [PosLit "q"]
      removeLiteral (PosLit "r") (toClause ["-p", "q"]) `shouldBe` Just [NegLit "p", PosLit "q"]
      removeLiteral (PosLit "p") (toClause ["-p"])      `shouldBe` Just []
      removeLiteral (NegLit "p") (toClause ["p"])       `shouldBe` Just []
      removeLiteral (PosLit "p") (toClause ["p"])       `shouldBe` Nothing

  -- describe "simplifyCNF" $ do
  --   it "should correctly simplify with unit clauses" $ do
  --     snd (simplifyCNF (toClause ["r"]:testCNF))  `shouldBe` [[]]
  --     simplifyCNF (toClause ["-r"]:testCNF)  `shouldBe` (toClause ["q", "-p", "-r"], [])
  --
  describe "findPureLiterals" $ do
    it "should correctly find pure literals" $ do
      findPureLiterals (toCNF [["r"], ["r"], ["-q"]])                 `shouldBe` toClause ["r", "-q"]
      findPureLiterals (toCNF [["-r"], ["r"], ["-q"]])                `shouldBe` toClause ["-q"]
      findPureLiterals (toCNF [[], ["r"], ["-q"]])                    `shouldBe` toClause ["r", "-q"]
      findPureLiterals (toCNF [[], ["r"], ["-q"]])                    `shouldBe` toClause ["r", "-q"]
      findPureLiterals (toCNF [["r", "q"], ["-q", "-r"]])             `shouldBe` toClause []
      findPureLiterals (toCNF [["p"], ["r", "q", "p"], ["-q", "-r"]]) `shouldBe` toClause ["p"]
