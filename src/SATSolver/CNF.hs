module SATSolver.CNF
(
  Literal (..)
, Clause
, CNF
, showCNF
, negateLit
)
where

import Data.List (intercalate)


data Literal = PosLit String
             | NegLit String
             deriving (Eq)
type Clause = [Literal]
type CNF = [Clause]

instance Show Literal where
  show (PosLit name) = name
  show (NegLit name) = "-" ++ name


showLiteral :: Literal -> String
showLiteral (PosLit name) = name
showLiteral (NegLit name) = "¬" ++ name

showClause :: Clause -> String
showClause literals = "(" ++ intercalate " ∨ " (map showLiteral literals) ++ ")"

showCNF :: CNF -> String
showCNF clauses = "[" ++ intercalate " ∧ " (map showClause clauses) ++ "]"

negateLit :: Literal -> Literal
negateLit (PosLit name) = NegLit name
negateLit (NegLit name) = PosLit name
