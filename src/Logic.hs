module Logic where

import Data


simplify :: LogicalFormula -> LogicalFormula
simplify phi = phi

eval :: LogicalFormula -> [(String, Bool)] -> Bool
eval (And ps) values = True
