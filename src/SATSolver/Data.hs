module SATSolver.Data
(
  LogicalFormula (..)
)
where

import Data.List (intercalate)


data LogicalFormula = Tru
                    | Fls
                    | Lit String
                    | Not LogicalFormula
                    | And [LogicalFormula]
                    | Or [LogicalFormula]
                    | Implies LogicalFormula LogicalFormula
                    | Equiv LogicalFormula LogicalFormula
                    deriving (Eq)

instance Show LogicalFormula where
  show Tru = "T"
  show Fls = "F"
  show (Lit name) = name
  show (Not p) = "¬" ++ show p
  show (And ps) = "(" ++ intercalate " ∧ " (map show ps) ++ ")"
  show (Or ps) = "(" ++ intercalate " ∨ " (map show ps) ++ ")"
  show (Implies p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
  show (Equiv p q) = "(" ++ show p ++ " <=> " ++ show q ++ ")"
