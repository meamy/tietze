-- | Test file for yaml configuration parsing.

module Main where

import System.Environment
import System.IO
import LafontExe.IO.Configs

-- | Parses and validates arguments before calling checkFiles.
main = do
    pname <- getProgName
    args <- getArgs
    if length args /= 1
    then putStrLn $ "usage: " ++ pname ++ " conf_file"
    else do
        res <- parseConfigYamlImpl $ head args
        case res of
            Left err   -> putStrLn $ printConfigErr err
            Right conf -> do
                putStrLn "Configuration File Summary"
                putStrLn $ "Generator File:   " ++ (show . generators) conf
                putStrLn $ "Relation File:    " ++ (show . relations) conf
                putStrLn $ "Derivation Files: " ++ (show . derivations) conf
                putStrLn $ "Obligations:      " ++ (show . obligations) conf
