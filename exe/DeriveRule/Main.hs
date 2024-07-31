-- | Command-line application to derive relations.

module Main where

import System.Environment
import System.IO
import Tietze.Common
import Tietze.Edit.Invert
import TietzeExe.IO.Configs
import TietzeExe.DeriveRule
import DeriveRule.CmdLn

-- | Helper method to pass configurations to doDerivation.
runTool :: Config -> DeriveRule -> IO ()
runTool conf args = doDerivation stdout gens rels query
    where gens  = generators conf
          rels  = relations conf
          query = DerQuery (relname args) [0..25] (cutoff args)

-- | Parses and validates arguments before calling doDerivation.
main = do
    args <- getCmdArgs
    res  <- parseConfigYaml $ configs args
    case res of
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> runTool conf args
