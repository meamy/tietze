-- | Command-line application to verify a list of derivation files.

module Main where

import ValidateDerivations.CmdLn
import System.IO
import System.Environment
import LafontExe.IO.Configs
import LafontExe.ValidateDerivations

-- | Helper method to pass configurations to validateDerivations.
runTool :: Config -> IO ()
runTool conf =
    if null ders
    then putStrLn "No derivations to prove."
    else validateDerivations stdout gens rels ders
    where gens = generators conf
          rels = relations conf
          ders = derivations conf

-- | Parses and validates arguments before calling validateDerivations.
main = do
    args <- getCmdArgs
    res  <- parseConfigYamlImpl $ configs args
    case res of
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> runTool conf
