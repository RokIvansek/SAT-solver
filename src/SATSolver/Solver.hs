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
findUnitClauses = removeDuplicates . map head . filter (\c -> length c == 1)

findPureLiterals :: CNF -> [Literal]
findPureLiterals phi = let allLits = concat phi
                           posLits = S.fromList $ map litName $ filter isPosLit allLits
                           negLits = S.fromList $ map litName $ filter isNegLit allLits
                           commonLits = S.intersection posLits negLits
                           purePos = map PosLit (S.toList $ S.difference posLits commonLits)
                           pureNeg = map NegLit (S.toList $ S.difference negLits commonLits)
                        in removeDuplicates $ purePos ++ pureNeg

removeLiteral :: Literal -> Clause -> Maybe Clause
removeLiteral l c
    | l `elem` c = Nothing
    | negateLit l `elem` c = Just $ delete (negateLit l) c
    | otherwise = Just c

simplifyCNF :: CNF -> ([Literal], CNF)
simplifyCNF phi = simplifyCNF' phi []
  where simplifyCNF' phi' vals = let units = findUnitClauses phi'
                                     pures = findPureLiterals phi'
                                 in if null units
                                    then (vals, phi')
                                    else let vals' = vals ++ units
                                             phi'' = foldl (\cs u -> mapMaybe (removeLiteral u) cs) phi' units
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
