-- | Command-line application to verify a list of derivation files.

module Main where

import System.IO
import System.Environment
import LafontExe.IO.Configs
import LafontExe.ValidateDerivations

-- | Helper method to pass configurations to validateDerivations.
runTool :: Config -> IO()
runTool conf =
    if null ders
    then putStrLn "No derivations to prove."
    else validateDerivations stdout gens rels ders
    where gens = generators conf
          rels = relations conf
          ders = derivations conf

-- | Parses and validates arguments before calling validateDerivations.
main = do
    pname <- getProgName
    args <- getArgs
    if length args \= 1
    then putStrLn $ "usage: " ++ pname ++ " conf_file"
    else do
        res <- parseConfigYamlImpl $ head args
        case res of
            Left err   -> putStrLn $ printConfigErr err
            Right conf -> runTool conf
