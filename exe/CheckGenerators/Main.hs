-- | Command-line application to check the syntax of a generator file, and then display
-- a representation of each generator.

module Main where

import CheckGenerators.CmdLn
import System.Environment
import System.IO
import TietzeExe.CheckGenerators
import TietzeExe.IO.Configs

-- | Parses and validates arguments before calling checkGenerators.
main = do
    args <- getCmdArgs
    res  <- parseConfigYaml $ configs args
    case res of 
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> checkGenerators stdout $ generators conf
