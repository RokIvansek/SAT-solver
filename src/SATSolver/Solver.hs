module SATSolver.Solver
--(
--  solve,
--  findPureLiterals,
--  simplifyCNF,
--  removeLiteral,
--  findUnitClauses,
--  removeDuplicates
--)
where

import SATSolver.CNF
import Data.List (delete, find)
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Set as S

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = S.toList . S.fromList

findUnitClauses :: CNF -> [Literal]
findUnitClauses = map head . filter (\c -> length c == 1)

plHelpPos :: Clause -> S.Set String -> S.Set String
plHelpPos clause a = foldl f1 a clause
    where f1 acc lit = case lit of
                        PosLit p -> S.insert p acc
                        NegLit p -> acc

plHelpNeg :: Clause -> S.Set String -> S.Set String
plHelpNeg clause b = foldl f2 b clause
    where f2 acc lit = case lit of
                        NegLit p -> S.insert p acc
                        PosLit p -> acc

findPureLiterals :: CNF -> ([String], [String])
findPureLiterals phi = let inter = S.intersection a b
                        in (S.toList $ S.difference a inter, S.toList $ S.difference b inter)
                            where a = foldl (\acc clause -> plHelpPos clause acc) S.empty phi
                                  b = foldl (\acc clause -> plHelpNeg clause acc) S.empty phi

removeLiteral :: Literal -> Clause -> Maybe Clause
removeLiteral l c
    | l `elem` c = Nothing
    | negateLit l `elem` c = Just $ delete (negateLit l) c
    | otherwise = Just c

simplifyCNF :: CNF -> ([Literal], CNF)
simplifyCNF phi = simplifyCNF' phi []
  where simplifyCNF' phi' vals = let unitsAndPurLits = units ++ purLits where
                                        units = removeDuplicates $ findUnitClauses phi'
                                        purLits = (map (\s -> PosLit s) posPurLit) ++ (map (\s -> NegLit s) negPurLit) where
                                            (posPurLit, negPurLit) = findPureLiterals phi'
                                 in if null unitsAndPurLits
                                        then (vals, phi')
                                        else let vals' = unitsAndPurLits ++ vals
                                                 phi'' = foldl (\cs u -> mapMaybe (removeLiteral u) cs) phi' unitsAndPurLits
                                                    in simplifyCNF' phi'' vals'

-- TODO: instead of this implement a good heuristic like MOM (Maximum Occurrences in clauses of Minimum Size)
randLiteral :: CNF -> Literal
randLiteral = head . fromJust . find (not . null)

solve :: CNF -> Maybe [Literal]
solve phi = let (vals', phi') = simplifyCNF phi
             in if null phi'
                then Just vals'
                else if [] `elem` phi'
                then Nothing
                else let p = randLiteral phi'
                      in case solve ([p]:phi') of
                              Just vals'' -> Just $ vals' ++ vals''
                              Nothing -> case solve ([negateLit p]:phi') of
                                              Just vals'' -> Just $ vals' ++ vals''
                                              Nothing -> Nothing
