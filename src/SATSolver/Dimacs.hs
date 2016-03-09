module SATSolver.Dimacs
(
  Dimacs (..)
, readDimacs
)
where

import SATSolver.Data
import Data.List()

data Dimacs = Dimacs { comments :: [String]
                     , nbvar :: Integer
                     , nbclauses :: Integer
                     , cnf :: LogicalFormula
                     } deriving (Show)

removeMarkers :: [String] -> [String]
removeMarkers = filter (not . null) . map tail

extractComments :: [String] -> [String]
extractComments = removeMarkers . filter (('c' ==) . head)

extractDescription :: [String] -> [String]
extractDescription = words . head . removeMarkers . filter (('p' ==) . head)

extractClauses :: [String] -> [String]
extractClauses = filter (\l -> head l /= 'c' && head l /= 'p')

parseAtom :: String -> LogicalFormula
parseAtom s = let atom = read s :: Integer
               in if atom < 0
                  then Not (Lit (show (abs atom :: Integer)))
                  else Lit (show (abs atom :: Integer))

parseClause :: String -> LogicalFormula
parseClause s = Or $ map parseAtom $ init $ words s

parseClauses :: [String] -> LogicalFormula
parseClauses s = And $ map parseClause s

readDimacs :: String -> IO Dimacs
readDimacs filename = do
  text <- readFile filename
  let ls = lines text
      comments' = extractComments ls
      (filetype:nbvar':nbclauses':_) = extractDescription ls
      cnf' = parseClauses $ extractClauses ls
   in if filetype /= "cnf"
      then error "Not a cnf dimacs file!"
      else return Dimacs {comments=comments', nbvar=read nbvar', nbclauses=read nbclauses', cnf=cnf'}
