-- | Command-line application to check the syntax of a relation file (with respect to a
-- generator file), and then displays a representation of each rewrite rule.

module Main where

import System.Environment
import System.IO
import LafontExe.CheckRelations
import LafontExe.IO.Configs

-- | Helper method to pass configurations to checkRelations.
runTool :: Config -> IO()
runTool conf = checkRelations stdout gens rels
    where gens = generators conf
          rels = relations conf

-- | Parses and validates arguments before calling checkRelations.
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
