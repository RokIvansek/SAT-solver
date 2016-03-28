module SATSolver.CNF
(
  Literal (..)
, isPosLit
, isNegLit
, litName
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


data Literal = PosLit Int
             | NegLit Int
             deriving (Eq, Ord)
type Clause = [Literal]
type CNF = [Clause]

instance Show Literal where
  show (PosLit name) = show name
  show (NegLit name) = "-" ++ show name

isPosLit,isNegLit :: Literal -> Bool
isPosLit (PosLit _) = True
isPosLit _ = False
isNegLit (NegLit _) = True
isNegLit _ = False

litName :: Literal -> Int
litName (PosLit name) = name
litName (NegLit name) = name

showLiteral :: Literal -> String
showLiteral (PosLit name) = show name
showLiteral (NegLit name) = "¬" ++ show name

showClause :: Clause -> String
showClause literals = "(" ++ intercalate " ∨ " (map showLiteral literals) ++ ")"

showCNF :: CNF -> String
showCNF clauses = "[" ++ intercalate " ∧ " (map showClause clauses) ++ "]"

readLiteral :: Int -> Literal
readLiteral atom = if atom < 0
                   then NegLit (abs atom)
                   else PosLit atom


negateLit :: Literal -> Literal
negateLit (PosLit name) = NegLit name
negateLit (NegLit name) = PosLit name


checkClause :: Clause -> [Literal] -> Bool
checkClause literals values = any (`elem` values) literals -- TODO: should we check if a literal even exists in the values (as either positive or negative)?

checkSolution :: CNF -> [Literal] -> Bool
checkSolution clauses values = all (`checkClause` values) clauses

