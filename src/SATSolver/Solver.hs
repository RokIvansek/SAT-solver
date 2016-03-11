module SATSolver.Solver
(
  solve
)
where

import SATSolver.CNF
import Data.List (delete, find)
import Data.Maybe (mapMaybe, fromJust)

findUnitClauses :: CNF -> [Literal]
findUnitClauses = map head . filter (\c -> length c == 1)

-- findPureLiterals :: CNF -> [Literal]
-- findPureLiterals _ = [] -- TODO

removeLiteral :: Literal -> Clause -> Maybe Clause
removeLiteral l c
    | l `elem` c = Nothing
    | negateLit l `elem` c = Just $ delete (negateLit l) c
    | otherwise = Just c

simplifyCNF :: CNF -> ([Literal], CNF)
simplifyCNF phi = simplifyCNF' phi []
  where simplifyCNF' phi' vals = let units = findUnitClauses phi'
                                 in if null units
                                    then (vals, phi')
                                    else let vals' = units ++ vals
                                             phi'' = foldl (\cs u -> mapMaybe (removeLiteral u) cs) phi' units
                                          in simplifyCNF' phi'' vals'

randLiteral :: CNF -> Literal
randLiteral = head . fromJust . find (not . null)


solve :: CNF -> Maybe [Literal]
solve phi = let (vals, phi') = simplifyCNF phi
             in if null phi'
                then Just vals
                else if [] `elem` phi'
                then Nothing
                else let p = randLiteral phi'
                      in case solve ([p]:phi') of
                              Just vals' -> Just vals'
                              Nothing -> solve ([negateLit p]:phi')
