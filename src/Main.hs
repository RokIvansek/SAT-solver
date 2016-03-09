module Main where

import System.Environment
import SATSolver


solve :: Dimacs -> Bool
solve (Dimacs {cnf=f}) = eval f [("1", True), ("5", False), ("3", True), ("4", False)]

main :: IO ()
main = do 
   (filename:_) <- getArgs
   d <- readDimacs filename
   putStrLn (show $ solve d)
