-- | Command-line application to query Elimination/Introduction rules.

module Main where

import Data.List.Split
import qualified Data.Set as Set
import System.Environment
import System.IO
import Lafont.Common
import Lafont.Edit.Invert
import LafontExe.IO.Configs
import LafontExe.QueryEIRules
import QueryEIRules.CmdLn

-- | Helper method to pass configurations to queryEIRules.
runTool :: Config -> QueryEIRules -> IO ()
runTool conf args = queryEIRules stdout gens rels ders query
    where gens  = generators conf
          rels  = relations conf
          ders  = derivations conf
          symbs = Set.fromList $ map toSymbol $ splitOn "," $ symbols args
          query = EIQuery symbs (leftInv args) (policy args)

-- | Parses and validates arguments before calling queryEIRules.
main = do
    args <- getCmdArgs
    res  <- parseConfigYamlImpl $ configs args
    case res of
        Left err   -> putStrLn $ printConfigErr err
        Right conf -> runTool conf args
