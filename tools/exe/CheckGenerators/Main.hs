-- | Command-line application to check the syntax of a generator file, and then display
-- a representation of each generator.

module Main where

import CheckGenerators.CmdLn
import System.Environment
import System.IO
import LafontExe.CheckGenerators
import LafontExe.IO.Configs

-- | Parses and validates arguments before calling checkGenerators.
main = do
    args <- getCmdArgs
    res  <- parseConfigYamlImpl $ configs args
    case res of 
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> checkGenerators stdout $ generators conf
