-- | Command-line application to check the syntax of a relation file (with respect to a
-- generator file), and then displays a representation of each rewrite rule.

module Main where

import CheckRelations.CmdLn
import System.Environment
import System.IO
import LafontExe.CheckRelations
import LafontExe.IO.Configs

-- | Helper method to pass configurations to checkRelations.
runTool :: Config -> IO ()
runTool conf = checkRelations stdout gens rels
    where gens = generators conf
          rels = relations conf

-- | Parses and validates arguments before calling checkRelations.
main = do
    args <- getCmdArgs
    res  <- parseConfigYamlImpl $ configs args
    case res of
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> runTool conf
