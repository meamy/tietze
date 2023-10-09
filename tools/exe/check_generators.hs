-- | Command-line application to check the syntax of a generator file, and then display
-- a representation of each generator.

module Main where

import System.Environment
import System.IO
import LafontExe.CheckGenerators
import LafontExe.IO.Configs

-- | Helper method to pass configurations to checkGenerators.
runTool :: Config -> IO()
runTool conf = checkGenerators stdout gens
    where gens = generators conf

-- | Parses and validates arguments before calling checkGenerators.
main = do
    pname <- getProgName
    args <- getArgs
    if length args /= 1
    then putStrLn $ "usage: " ++ pname ++ " conf_file"
    else do
        res <- parseConfigYamlImpl $ head args
        case res of
            Left err   -> putStrLn $ printConfigErr err
            Right conf -> runTool conf
