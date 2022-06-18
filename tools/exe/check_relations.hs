-- | Command-line application to check the syntax of a relation file (with respect to a
-- generator file), and then displays a representation of each rewrite rule.

module Main where

import System.Environment
import LafontExe.CheckRelations

-- | Parses and validates arguments before calling checkFiles.
main = do
    pname <- getProgName
    args <- getArgs
    if (not ((length args) == 2))
    then putStr ("usage: " ++ pname ++ " gen_file rel_file\n")
    else checkRelations (args !! 0) (args !! 1)
