module SATSolver.CNF
(
  Literal (..)
, Clause
, CNF
, showCNF
, showLiteral
, readLiteral
, negateLit
, checkSolution
)
where

import Data.List (intercalate)


data Literal = PosLit String
             | NegLit String
             deriving (Eq, Ord)
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

readLiteral :: String -> Literal
readLiteral atom = if head atom == '-'
                   then NegLit (tail atom)
                   else PosLit atom


negateLit :: Literal -> Literal
negateLit (PosLit name) = NegLit name
negateLit (NegLit name) = PosLit name


checkClause :: Clause -> [Literal] -> Bool
checkClause literals values = any (`elem` values) literals -- TODO: should we check if a literal even exists in the values (as either positive or negative)?

checkSolution :: CNF -> [Literal] -> Bool
checkSolution clauses values = all (`checkClause` values) clauses

