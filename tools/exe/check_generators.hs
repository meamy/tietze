-- | Command-line application to check the syntax of a generator file, and then display
-- a representation of each generator.

module Main where

import System.Environment
import LafontExe.CheckGenerators

-- | Parses and validates arguments before calling checkGenerators.
main = do
    pname <- getProgName
    args <- getArgs
    if (not ((length args) == 1))
    then putStr ("usage: " ++ pname ++ " filename\n")
    else checkGenerators (head args)
