# SAT-solver
Implementation of a SAT solver in Haskell.

# Running
In order to run, clone the repository, and make sure you have `cabal` installed on your system.

Navigate to the root directory of the project and run `cabal install` to install the library.

## Testing the library
In order to test the library via the repl, run `cabal repl` from the root directory of the project.

## Running unit tests
If your cabal's package list is not up to date, run `cabal update`, and then install hspec by running `cabal install hspec`.

After you have hspec installed, you can run `cabal test` from inside root directory of the project which should run all the tests from [test/](./test/) directory.
