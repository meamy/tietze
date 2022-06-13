-- | Command-line application to check the syntax of a generator file, and then display
-- a representation of each generator.

module Main where

import System.Directory
import System.Environment
import DyadicRewrite.Generators.Semantics
import DyadicRewrite.IO.LineBasedLogging
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.GeneratorFile

-- | Consumes a generator name, its showable semantics, and an accumulated string.
-- Returns a textual representation representation of the name and semantics, appended to
-- the end of the accumulated string.
logGenerator :: (Show a) => (String, Maybe a) -> String -> String
logGenerator (name, (Just semv)) str = str ++ name ++ " := " ++ (show semv) ++ "\n"
logGenerator (name, Nothing)     str = str ++ name ++ " := (no semantic value)\n"

-- | Converts a dictionary of generators to a textual representation.
logGenerators :: (Show a) => SemModel -> GenDict a -> String
logGenerators sem dict = foldGens logGenerator semstr dict
    where semstr = "Semantic Model : " ++ (show sem) ++ "\n"

-- | Parses all generators and returns their textual representation if possible. If not,
-- then an error message is returned.
processGenFile :: String -> [String] -> String
processGenFile fname lines =
    case (parseGenFileAsDict lines 0) of
        Left (errLn, err)               -> (logEitherMsg fname errLn err)
        Right (MonoidalGenSummary dict) -> logGenerators MonoidalSem dict 

-- | Validates file before calling processGenFile.
readGenFile :: String -> IO ()
readGenFile fname = do
    exists <- doesFileExist fname
    if exists
    then do
        content <- readFile fname
        putStr (processGenFile fname (lines content))
    else putStr ("File does not exist: " ++ fname ++ "\n")

-- | Parses and validates arguments before calling readGenFile.
main = do
    pname <- getProgName
    args <- getArgs
    if (not ((length args) == 1))
    then putStr ("usage: " ++ pname ++ " filename\n")
    else readGenFile (head args)
