module Logic where

import Data


simplify :: LogicalFormula -> LogicalFormula
simplify (Not (And ps)) = Or $ map (simplify . Not) ps
simplify (Not (Or ps)) = And $ map (simplify . Not) ps
simplify (Not (Not phi)) = phi
simplify (Not phi) = Not (simplify phi)
simplify phi = phi

eval :: LogicalFormula -> [(String, Bool)] -> Bool
eval (And ps) values = True
