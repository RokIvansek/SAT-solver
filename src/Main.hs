module Main where

import System.Environment
import SATSolver
import Data.List (intercalate)


main :: IO ()
main = do 
   (filename:_) <- getArgs
   d <- readDimacs filename
   putStrLn $ "Solving for " ++ showCNF (cnf d)
   case solveDimacs d of
        Just solution -> putStrLn $ intercalate " " (map show solution)
        Nothing -> putStrLn "No solution"
