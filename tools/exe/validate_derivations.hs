-- | Command-line application to verify a list of derivation files.

module Main where

import System.Environment
import LafontExe.ValidateDerivations

-- | Parses and validates arguments before calling checkFiles.
main = do
    pname <- getProgName
    args <- getArgs
    if (not ((length args) >= 3))
    then putStr ("usage: gen_file rel_file deriv_file ... deriv_file\n")
    else validateDerivations (args !! 0) (args !! 1) (tail (tail args))
