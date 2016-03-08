module Logic where

import Data


simplify :: LogicalFormula -> LogicalFormula
simplify (Not (And ps)) = Or $ map (simplify . Not) ps
simplify (Not (Or ps)) = And $ map (simplify . Not) ps
simplify (Not (Not p)) = p
simplify (Not p) = Not (simplify p)
simplify (And ps) = And $ map simplify ps
simplify (Or ps) = Or $ map simplify ps
simplify (Implies p q) = Implies (simplify p) (simplify q)
simplify (Equiv p q) = Equiv (simplify p) (simplify q)
simplify p = p

eval :: LogicalFormula -> [(String, Bool)] -> Bool
eval (And ps) values = True
