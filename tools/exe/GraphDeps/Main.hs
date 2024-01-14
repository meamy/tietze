-- | Command-line application to relation inversion.

module Main where

import System.Environment
import System.IO
import Lafont.Common
import LafontExe.GraphDeps
import LafontExe.IO.Configs
import GraphDeps.CmdLn

-- | Helper method to pass configurations to graphDeps.
runTool :: Config -> GraphDeps -> IO ()
runTool conf args = graphDeps stdout gens rels ders
    where gens  = generators conf
          rels  = relations conf
          ders  = derivations conf

-- | Parses and validates arguments before calling graphDeps.
main = do
    args <- getCmdArgs
    res  <- parseConfigYamlImpl $ configs args
    case res of
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> runTool conf args
