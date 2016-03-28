module Main where

import System.Environment
import SATSolver


readSolution :: String -> IO [Literal]
readSolution filename = do
  text <- readFile filename
  return (map (readLiteral . read) $ words text) -- TODO: check that all literals are covered and no literal appears more than once

showHelp :: IO ()
showHelp = putStrLn "Usage: SATSolver [path/to/dimacs/] {[path/to/solution/]}"

solveDimacs :: String -> IO ()
solveDimacs fndimacs = do
   d <- readDimacs fndimacs
   -- putStrLn $ "Solving " ++ showCNF (cnf d)
   case solve (cnf d) of
        Just solution -> putStrLn $ unwords $ map show solution
        Nothing -> putStrLn "No solution"

checkDimacs :: String -> String -> IO ()
checkDimacs fndimacs fnsolution = do
   d <- readDimacs fndimacs
   s <- readSolution fnsolution
   putStrLn $ "Checking solution for " ++ showCNF (cnf d)
   putStrLn $ "With " ++ show s
   putStrLn $ "Result: " ++ show (checkSolution (cnf d) s)


main :: IO ()
main = do 
   args <- getArgs
   case args of
        [fndimacs] -> solveDimacs fndimacs
        [fndimacs, fnsolution] -> checkDimacs fndimacs fnsolution
        _ -> showHelp
