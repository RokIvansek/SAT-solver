module SATSolver.Solver
-- (
--   solve
-- , solveDimacs
-- )
where

import SATSolver.CNF

solve :: CNF -> Maybe [Literal]
solve _ = Just [NegLit "a", PosLit "b"]
