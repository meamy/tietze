-- | Command-line application to convert derivations to LaTeX.

module Main where

import System.Environment
import System.IO
import TietzeExe.ToLatex
import TietzeExe.IO.Configs
import ToLatex.CmdLn

-- | Helper method to pass configurations to doInversion.
runTool :: Config -> ToLatex -> IO ()
runTool conf args = toLatex stdout gens rels ders
    where gens  = generators conf
          rels  = relations conf
          ders  = derivations conf

-- | Parses and validates arguments before calling doInversion.
main = do
    args <- getCmdArgs
    res  <- parseConfigYaml $ configs args
    case res of
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> runTool conf args
