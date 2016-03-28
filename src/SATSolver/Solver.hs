module SATSolver.Solver
 (
   solve,
   counts,
   count,
   maxo,
   moms,
   mams,
   jw
 )
where

import SATSolver.CNF
import Data.List (delete, find, maximumBy, zipWith, nub)
import Data.Maybe (mapMaybe, fromJust)
import Data.Function (on)
import Data.Set (toList, fromList, intersection, difference)
import qualified Data.Map as Map
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

removeLitFromClause :: Literal -> Clause -> Maybe Clause
removeLitFromClause l c
    | l `elem` c = Nothing
    | negateLit l `elem` c = Just $ delete (negateLit l) c
    | otherwise = Just c

removeLit :: Literal -> CNF -> CNF
removeLit l = mapMaybe (removeLitFromClause l)

simplifyCNF :: CNF -> ([Literal], CNF)
simplifyCNF phi = simplifyCNF' phi []
  where simplifyCNF' phi' vals' = let units = findUnitClauses phi'
                                      pures = findPureLiterals phi'
                                      toRemove = if null units then pures else units
                                   in if null toRemove
                                      then (vals', phi')
                                      else let vals'' = vals' ++ toRemove
                                               phi'' = foldl (flip removeLit) phi' toRemove
                                            in simplifyCNF' phi'' vals''

-- heuristics for selecting the optimal guess
counts :: Ord a => [a] -> [(a, Int)]
counts = Map.toList . Map.fromListWith (+) . map (\n -> (n, 1))

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

maxo :: CNF -> Literal
maxo phi = let literals = concat phi
               nameCounts = counts $ map litName literals
               maxLit = maximumBy (compare `on` snd) nameCounts
               (maxName, maxCount) = maxLit
               posCount = count (PosLit maxName) literals
            in if posCount > maxCount - posCount
               then PosLit maxName
               else NegLit maxName

moms :: CNF -> Literal
moms phi = let minLen = minimum $ map length phi
               minClauses = filter (\c -> length c == minLen) phi
            in maxo minClauses

mams :: CNF -> Literal
mams phi = let minClsSize = minimum $ map length phi
               minSizeClss = filter (\x -> length x == minClsSize) phi
               smallLits = concat minSizeClss
               momss = Map.toList . Map.fromListWith (+) $ counts $ map litName smallLits
               allLits = concat phi
               maxos = counts $ map litName allLits
               mamss = Map.toList . Map.fromListWith (+) $ (momss ++ maxos)
               maxLit = maximumBy (compare `on` snd) mamss
               (maxName, maxCount) = maxLit
               posCount = (count (PosLit maxName) allLits) + (count (PosLit maxName) smallLits)
           in if posCount > maxCount - posCount
               then PosLit maxName
               else NegLit maxName

jw :: CNF -> Literal
jw phi = let lits = map (\xs -> map litName xs) phi
             litsInClss = map nub lits
             lengths = map length phi
             wLitsInClss = zipWith (\xs x -> (map (\y -> (y, 2^^(-x))) xs)) litsInClss lengths
             jws = Map.toList . Map.fromListWith (+) $ concat wLitsInClss
             best = maximumBy (compare `on` snd) jws
         in PosLit $ fst best

up :: Literal -> CNF -> Int
up l = length . findUnitClauses . removeLit l

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
