-- | Test file for yaml configuration parsing.

module Main where

import           Data.List.NonEmpty
import qualified Data.Map             as Map
import           System.Environment
import           System.Exit
import           System.IO
import           Lafont.Format.GraphViz
import           LafontExe.IO.Configs
import           YamlTest.CmdLn

-- |
checkConfigs :: String -> IO ()
checkConfigs fn = do
    res <- parseConfigYamlImpl fn
    case res of
        Left err -> do
            putStrLn $ "[Configuration Error] " ++ printConfigErr err
            exitFailure
        Right conf -> do
            putStrLn "Configuration File Summary"
            putStrLn $ "  Generator File:   " ++ (show . generators) conf
            putStrLn $ "  Relation File:    " ++ (show . toList . relations) conf
            putStrLn $ "  Derivation Files: " ++ (show . derivations) conf
            putStrLn $ "  Obligations:      " ++ (show . obligations) conf

-- |
checkStyle :: String -> IO ()
checkStyle fn = do
    res <- parseStyleYamlImpl fn
    case res of
        Left err -> do
            putStrLn $ "[Style Error] " ++ printConfigErr err
            exitFailure
        Right sty -> do
            putStrLn "Style File Summary"
            putStrLn $ "  Color: " ++ (show . color) sty

-- | Parses and validates arguments before calling parseConfigYamlImpl.
main = do
    args <- getCmdArgs
    -- Prints configurations by default.
    checkConfigs $ configs args
    -- If a style is provided, and this line is reached, then the style is printed.
    case style args of
        Just sty -> do
            putStrLn ""
            checkStyle sty
        Nothing -> return ()
