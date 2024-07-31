-- | Command-line application to find meets of lhs and rhs of a relation.

module Main where

import System.Environment
import System.IO
import Tietze.Common
import Tietze.Edit.Invert
import TietzeExe.IO.Configs
import TietzeExe.FindMeet
import FindMeet.CmdLn

-- | Helper method to pass configurations to doFindMeet.
runTool :: Config -> FindMeet -> IO ()
runTool conf args = doFindMeet stdout gens rels query
    where gens  = generators conf
          rels  = relations conf
          query = DerQuery (relname args) [0..25] (cutoff args)

-- | Parses and validates arguments before calling doFindMeet.
main = do
    args <- getCmdArgs
    res  <- parseConfigYaml $ configs args
    case res of
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> runTool conf args
