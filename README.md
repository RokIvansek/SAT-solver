# SAT-solver
Implementation of a SAT solver in Haskell.

# Running
In order to run, this project uses Cabal to solve dependencies and generate binaries. To compile the binaries, running `cabal build` from the root directory of this project should generate `SATSolver` binary inside `dist/build/SATSolver/` directory.

You can then run the binary by calling it with a single argument, representing the path to the [dimacs](http://www.satcompetition.org/2009/format-benchmarks2009.html) file with formula you wish to solve. This generates either a solution in form of literals, or string "No solution" in case formula is not satisfiable. You can check the obtained solution by saving it into a separate file and invoking the program again with 2 arguments: path to dimacs file with the formula, and path to the file containing the solution. This should produce either "True" if the solution satisfies the formula, and "False" otherwise.

Note that the current implementation expects a "nicely" formated dimacs file on the input: each line not starting with `c` or `p` is assumed to represent a separate clause in the formula, meaning each clause should be placed on its own line, ending with `␣0\n`, and no line should be empty.

# Example
As an example, to find the solution to a problem in [test/dimac/example.txt](test/dimac/example.txt), you would use:

    $ ./dist/build/SATSolver/SATSolver test/dimac/example.txt
    4 -3

meaning positive value for literal 4 and negative value for literal 3 represent a solution to the input. You can test this solution by running

    $ ./dist/build/SATSolver/SATSolver test/dimac/example.txt > solution
    $ ./dist/build/SATSolver/SATSolver test/dimac/example.txt solution
    Checking solution for [(1 ∨ ¬5 ∨ 4) ∧ (¬1 ∨ 5 ∨ 3 ∨ 4) ∧ (¬3 ∨ ¬4)]
    With [4,-3]
    Result: True

meaning our solution does in fact satisfy the formula.

# Testing
We tested our SAT solver on different dimac files. Some of them can be found in test/dimac folder. In addition to that we wrote a script that generates a .dimac file for the famous [n-queens puzzle](https://en.wikipedia.org/wiki/Eight_queens_puzzle) problem of specifiyed size.
The python script for queens along with some test examples can be found in test/queens folder.

For the ONE example we chose the "19x19queens.txt" file. It represents a formula with 361 variables and 10735 clauses. The solver finds the solution in cca 20 seconds. 

# Details
This is a very basic SAT solver using DPLL algorithm. When the algorithm encounters the branching (guessing) step, it uses the so called SUP heuristics (using MAXO, MOMS, MAMS, JW and UP heuristics under the hood) to determine the optimal free literal to branch on next. The heuristic is described in greater detail in the paper [Learning to Select Branching Rules in the DPLL Procedure for Satisfiability](https://www.cs.duke.edu/research/AI/RLSAT/sat2001.pdf) by Lagoudakis and Littman.
