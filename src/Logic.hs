module Logic where

import Data


simplify :: LogicalFormula -> LogicalFormula
simplify (Not (And ps)) = Or $ map (simplify . Not) ps
simplify (Not (Or ps)) = And $ map (simplify . Not) ps
simplify (Not (Not p)) = simplify p
simplify (Not Tru) = Fls
simplify (Not Fls) = Tru
simplify (Not p) = Not (simplify p)
simplify (And ps) = And $ map simplify ps
simplify (Or ps) = Or $ map simplify ps
simplify (Implies p q) = Implies (simplify p) (simplify q)
simplify (Equiv p q) = Equiv (simplify p) (simplify q)
simplify p = p

eval :: LogicalFormula -> [(String, Bool)] -> Bool
eval Tru values = True
eval Fls values = False
eval (Lit name) values = case lookup name values of
                              Just v -> v
                              Nothing -> error ("No value for atom " ++ name)
eval (Not p) values = not $ eval p values
eval (And ps) values = all id $ map (flip eval values) ps
eval (Or ps) values = any id $ map (flip eval values) ps
eval (Implies p q) values = eval (Or [Not p, q]) values
eval (Equiv p q) values = eval (And [Implies p q, Implies q p]) values
