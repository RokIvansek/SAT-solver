module SATSolver.Solver
(
  solve
)
where

import SATSolver.CNF
import Data.List (delete, find)
import Data.Maybe (mapMaybe, fromJust)
import Data.Set (toList, fromList, intersection, difference)
import Data.Generics.Aliases (orElse)
import Control.Applicative ((<$>), (<*>))

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

findUnitClauses :: CNF -> [Literal]
findUnitClauses = removeDuplicates . map head . filter (\c -> length c == 1)

findPureLiterals :: CNF -> [Literal]
findPureLiterals phi = let allLits = concat phi
                           posLits = fromList $ map litName $ filter isPosLit allLits
                           negLits = fromList $ map litName $ filter isNegLit allLits
                           intLits = intersection posLits negLits
                           purePos = map PosLit (toList $ difference posLits intLits)
                           pureNeg = map NegLit (toList $ difference negLits intLits)
                        in purePos ++ pureNeg

removeLiteral :: Literal -> Clause -> Maybe Clause
removeLiteral l c
    | l `elem` c = Nothing
    | negateLit l `elem` c = Just $ delete (negateLit l) c
    | otherwise = Just c

simplifyCNF :: CNF -> ([Literal], CNF)
simplifyCNF phi = simplifyCNF' phi []
  where simplifyCNF' phi' vals' = let units = findUnitClauses phi'
                                      pures = findPureLiterals phi'
                                      toRemove = if null units then pures else units
                                   in if null toRemove
                                      then (vals', phi')
                                      else let vals'' = vals' ++ toRemove
                                               phi'' = foldl (\cs u -> mapMaybe (removeLiteral u) cs) phi' toRemove
                                            in simplifyCNF' phi'' vals''

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
                      in (++) <$> Just vals' <*> orElse (solve ([p]:phi')) (solve ([negateLit p]:phi'))
