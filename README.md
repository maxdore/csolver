# Cube solver for Cubical Agda

This repository contains an experimental implementation of a tactic for Cubical
Agda deriving `hcomp` terms automatically. An executable can be built with
`stack build`.

## Usage

You can check a file with `stack exec csolver-exe FILE.agda`. The solver will
then call `agda` with the `--interaction` flag to extract goals from `FILE.agda`
and run the solver on each goal. 

Parsing the output from Agda is not very robust and might fail, you can instead
encode the goal directly internally in the solver. See `src/Examples.hs` for
some examples of such proof tasks. You can run the solver on a problem by
loading `app/Main.hs` in the Haskell REPL and runnning, e.g., `runSolve invGoal`.

If successful, the solver will print Agda code that can be pasted in the
corresponding goal in the Agda file.

## Code structure and working principle

The data types used for representing cubes can be found in `src/Data.hs`, the
main solver lies in `src/Core.hs`. Following [Schrijvers Stuckey Wadler
2009](https://homepages.inf.ed.ac.uk/wadler/papers/constraints/constraints.pdf),
we implement a monadic constraint solver for finding cubes.


